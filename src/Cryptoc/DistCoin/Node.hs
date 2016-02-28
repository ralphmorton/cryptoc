{-# LANGUAGE OverloadedStrings #-}

module Cryptoc.DistCoin.Node(
    createGenesisNode,
    createNode,
    destroyNode,
    getBlockChain
) where

import Cryptoc.BadCrypto
import Cryptoc.DistCoin.Types
import Cryptoc.DistCoin.Network
import Cryptoc.DistCoin.BlockChain

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Lens ((&), (.~), at)
import Control.Monad (forever)
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Time.Clock.POSIX (getPOSIXTime)

--
-- Notes:
-- ID generation is not safe in general
--

data NodeData = NodeData {
    ndID :: Int,
    ndNetwork :: Network,
    ndRewardPK :: PublicKey,
    ndBlockChain :: TVar BlockChain,
    ndBlockQueue :: TQueue Block,
    ndTransQueue :: TQueue SignedTransaction
}

--
-- Create a genesis node, with a single genesis block
--

createGenesisNode :: Network -> PrivateKey -> PublicKey -> PublicKey -> [(PublicKey, Double)] -> IO Node
createGenesisNode net daemonSK daemonPK rewardPK genesisOutputs = do
    sigTrans <- createTransaction daemonSK daemonPK ("", 0, 0) genesisOutputs
    genesisBlock <- generateBlock [sigTrans] Nothing
    let genesisHash = hashBlock genesisBlock
    let blockChain = (genesisHash, M.fromList [(genesisHash, genesisBlock)])
    createNodeWithBlockChain net blockChain rewardPK

--
-- Create a node, copying the blockchain from another node
--

createNode :: Network -> PublicKey -> Node -> IO Node
createNode net rewardPK fromNode = do
    blockChain <- atomically . readTVar . nBlockChain $ fromNode
    createNodeWithBlockChain net blockChain rewardPK

--
-- Create a node given a blockchain
--

createNodeWithBlockChain :: Network -> BlockChain -> PublicKey -> IO Node
createNodeWithBlockChain net blockChain rewardPK = do
    nid <- fmap (round . (*1000)) getPOSIXTime
    tqb <- atomically newTQueue
    tqt <- atomically newTQueue
    tbc <- atomically . newTVar $ blockChain
    registerNodeOnNetwork net nid tqb tqt
    let nodeData = NodeData nid net rewardPK tbc tqb tqt
    tid <- forkIO $ runNode nodeData
    return $ Node nid tid tbc tqb tqt

--
-- Destroy a node
--

destroyNode :: Network -> Node -> IO ()
destroyNode net node = do
    killThread $ nWorker node
    unregisterNodeOnNetwork net $ nID node

--
-- Get a nodes block chain
--

getBlockChain :: Node -> IO BlockChain
getBlockChain = atomically . readTVar . nBlockChain

--
-- Register a node on the network, identified by its ID
-- This ID is not used for consensus, but rather simulates an actualy network address
--

registerNodeOnNetwork :: Network -> Int -> TQueue Block -> TQueue SignedTransaction -> IO ()
registerNodeOnNetwork (Network netNodes) nid bQueue tQueue = atomically $ do
    nmap <- readTVar netNodes
    let nmap' = nmap & at nid .~ (Just (bQueue, tQueue))
    writeTVar netNodes nmap'

--
-- Unregister a node on the network
--

unregisterNodeOnNetwork :: Network -> Int -> IO ()
unregisterNodeOnNetwork (Network netNodes) nid = atomically $ do
    nmap <- readTVar netNodes
    let nmap' = nmap & at nid .~ Nothing
    writeTVar netNodes nmap'

--
-- Run a node
--

runNode :: NodeData -> IO ()
runNode nd = do
    forkIO $ runReaderT updateBlockChain nd
    forkIO $ runReaderT mine nd
    forever $ threadDelay 1000000

updateBlockChain :: ReaderT NodeData IO ()
updateBlockChain = forever $ do
    n <- ask
    mblock <- lift . atomically $ tryReadTQueue (ndBlockQueue n)
    maybe (return ()) appendBlock mblock
    lift $ threadDelay 1000000

appendBlock :: Block -> ReaderT NodeData IO ()
appendBlock block = ask >>= \n -> lift . atomically $ do
    let tBlockChain = ndBlockChain n
    blockChain@(h, m) <- readTVar tBlockChain
    case (checkBlock block blockChain) of
        False -> return ()
        True -> do
            let h' = hashBlock block
            let m' = M.insert h' block m
            let c1 = (h, m')
            let c2 = (h', m')
            case (chainLength c1 > chainLength c2) of
                True -> writeTVar tBlockChain c1
                False -> writeTVar tBlockChain c2

mine :: ReaderT NodeData IO ()
mine = forever $ do
    n <- ask
    blockChain <- lift . atomically $ readTVar (ndBlockChain n)
    tx <- lift . atomically $ takeAllTransactions (ndTransQueue n)
    let tx' = filter (transactionValid blockChain) tx
    let bch = fst blockChain
    let net = ndNetwork n
    block <- lift $ generateBlock tx' (Just bch)
    lift $ broadcastBlock net block
    lift $ threadDelay 1000000

addTransReward :: PublicKey -> SignedTransaction -> SignedTransaction
addTransReward rewardPK (Transaction sender input outputs, sig) = (trans', sig)
    where
    trans' = Transaction sender input $ (rewardPK, transactionReward):outputs

takeAllTransactions :: TQueue SignedTransaction -> STM [SignedTransaction]
takeAllTransactions q = do
    mt <- tryReadTQueue q
    case mt of
        Nothing -> return []
        Just t -> takeAllTransactions q >>= return . (t:)



















--
