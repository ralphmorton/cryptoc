
module Cryptoc.DistCoin.Types where

import Cryptoc.BadCrypto

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

type BlockHash = B.ByteString

type CoinAddress = (BlockHash, Int, Int)

--
-- Simulate a P2P network
--

data Network = Network (TVar (M.Map Int (TQueue Block, TQueue SignedTransaction)))

--
-- A node in the network
--

data Node = Node {
    nID :: Int,
    nWorker :: ThreadId,
    nBlockChain :: TVar BlockChain,
    nBlockQueue :: TQueue Block,
    nTransQueue :: TQueue SignedTransaction
}

--
-- Blockchain representation
-- For real implementations a merkle tree/patricia tree sort of thing would be used
--

type BlockChain = (BlockHash, M.Map BlockHash Block)

data Block = Block {
    bNonce :: Int,
    bTransactions :: [SignedTransaction],
    bParent :: Maybe BlockHash
} deriving (Eq, Show)

data Transaction = Transaction {
    tSender :: PublicKey,
    tInputCoin :: CoinAddress,
    tOutputCoins :: [(PublicKey, Double)]
} deriving (Eq, Show)

type SignedTransaction = (Transaction, Sig)
