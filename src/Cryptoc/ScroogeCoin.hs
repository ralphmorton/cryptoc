{-# LANGUAGE OverloadedStrings #-}

module Cryptoc.ScroogeCoin(
    module Cryptoc.BadCrypto,
    CoinID,
    Address,
    BlockChain(..),
    BlockData(..),
    Transaction(..),
    Scrooge,
    createAScrooge,
    scroogePubKey,
    scroogeBlockChain,
    giveCoinsTo,
    scroogeTransact,
    createTransaction,
    getCoinInfo,
    verifyBlockChain
) where

import Cryptoc.BadCrypto

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Debug.Trace

type CoinID = (Int, Int)
type Address = PublicKey

data BlockChain = Block {
    bcBlockID :: Int,
    bcBlockData :: BlockData,
    bcBlockSig :: Sig,
    bcHashPointer :: Maybe (B.ByteString, BlockChain)
} deriving Show

data BlockData = BlockData Transaction Sig deriving Show

data Transaction = Transaction {
    trInputCoinID :: CoinID,
    trOutputs :: [(Address, Double)]
} deriving Show

data Scrooge = Scrooge {
    scPublicKey :: PublicKey,
    scPrivateKey :: PrivateKey,
    scBlockChain :: TVar BlockChain
}

createAScrooge :: Double -> IO Scrooge
createAScrooge bal = do
    (pk,sk) <- genKeyPair
    (trans, tsig) <- createTransaction sk (-1,0) [(pk, bal)]
    let bdata = BlockData trans tsig
    (Just bsig) <- sign sk $ assembleBlockSigData 0 bdata Nothing
    let bc = Block {
        bcBlockID = 0,
        bcBlockData = bdata,
        bcBlockSig = bsig,
        bcHashPointer = Nothing
    }
    tv <- atomically $ newTVar bc
    return $ Scrooge pk sk tv

scroogePubKey :: Scrooge -> PublicKey
scroogePubKey = scPublicKey

scroogeBlockChain :: Scrooge -> IO BlockChain
scroogeBlockChain = atomically . readTVar . scBlockChain

--
-- Scrooge is generous
--

giveCoinsTo :: Scrooge -> Address -> Double -> IO Bool
giveCoinsTo sc pk val = do
    let scroogePK = scPublicKey sc
    let scroogeSK = scPrivateKey sc
    Just (scroogeBalance, _) <- getBalance sc scroogePK
    case scroogeBalance <= val of
        True -> return False
        False -> do
            (trans, tsig) <- createTransaction scroogeSK (0, 0) [(pk, val), (scroogePK, scroogeBalance - val)]
            scroogeTransact sc trans tsig scroogePK

--
-- Get balance
--

getBalance :: Scrooge -> Address -> IO (Maybe (Double, CoinID))
getBalance sc pk = do
    bc <- atomically . readTVar . scBlockChain $ sc
    return $ findFundsOwnedBy bc pk

findFundsOwnedBy :: BlockChain -> Address -> (Maybe (Double, CoinID))
findFundsOwnedBy (Block bid (BlockData trans _) _ bpointer) pk = case (L.lookup pk cOutputs) of
    Nothing -> maybe Nothing (flip findFundsOwnedBy pk . snd) bpointer
    Just outp -> Just outp
    where
    cOutputs = zipWith (\(addr, val) ix -> (addr, (val, (bid, ix)))) (trOutputs trans) [0..]

--
-- Scrooge is pretty honest and transactions are only allowed if the sender actually owns the coins
-- NB: the STM model here isnt ideal, but fine for this toy code
--

scroogeTransact :: Scrooge -> Transaction -> Sig -> Address -> IO Bool
scroogeTransact sc trans sig pk = do
    let tv = scBlockChain sc
    bc@(Block bid bdata bsig bpointer) <- atomically $ readTVar tv
    case (verifyTransaction bc trans sig pk) of
        False -> return False
        True -> do
            let trans' = addRecipientBalances bc pk trans
            let bhash = fmap fst bpointer
            let bhash' = hash $ assembleBlockSigData bid bdata bhash
            let bdata' = BlockData trans' sig
            let bid' = bid + 1
            (Just bsig') <- sign (scPrivateKey sc) . assembleBlockSigData bid' bdata' $ Just bhash'
            let bc' = Block bid' bdata' bsig' $ Just (bhash', bc)
            atomically $ writeTVar tv bc'
            return True

addRecipientBalances :: BlockChain -> Address -> Transaction -> Transaction
addRecipientBalances bc pk (Transaction cid outputs) = Transaction cid $ fmap (addRecipientBalance bc pk) outputs

addRecipientBalance :: BlockChain -> Address -> (Address, Double) -> (Address, Double)
addRecipientBalance bc pk (pkr, val)
    | pk /= pkr = (pkr, val + bal)
    | otherwise = (pkr, val)
    where bal = maybe 0 fst $ findFundsOwnedBy bc pkr

verifyTransaction :: BlockChain -> Transaction -> Sig -> Address -> Bool
verifyTransaction bc trans sig pk = coinOwnerOK && transSigOK
    where
    outputVal = sum . fmap snd . trOutputs $ trans
    coinInfo = getCoinInfo bc $ trInputCoinID trans
    coinOwnerOK = maybe False (\(owner, val) -> owner == pk && val == outputVal) coinInfo
    transSigOK = verify pk (B.pack . show $ trans) sig

--
-- Create a transaction
--

createTransaction :: PrivateKey -> CoinID -> [(Address, Double)] -> IO (Transaction, Sig)
createTransaction sk coinID outputs = do
    let trans = Transaction coinID outputs
    let transS = B.pack . show $ trans
    (Just sig) <- sign sk transS
    return (trans, sig)

--
-- Find the owner and value of a coin in a blockchain
--

getCoinInfo :: BlockChain -> CoinID -> Maybe (Address, Double)
getCoinInfo (Block bid bdata bsig bpointer) (blockID, outputIX)
    | bid == blockID = getCoinInfoInBlockData outputIX bdata
    | otherwise = maybe Nothing (flip getCoinInfo (blockID, outputIX)) $ fmap snd bpointer

getCoinInfoInBlockData :: Int -> BlockData -> Maybe (Address, Double)
getCoinInfoInBlockData ix (BlockData trans _)
    | ix >= length outputs = Nothing
    | otherwise = Just $ outputs !! ix
    where outputs = trOutputs trans

--
-- Verify the entire block chain. If Scrooge says its OK, its OK
--

verifyBlockChain :: BlockChain -> PublicKey -> Bool
verifyBlockChain (Block bid bdata bsig Nothing) scroogePK = verify scroogePK (assembleBlockSigData bid bdata Nothing) bsig
verifyBlockChain (Block bid bdata bsig (Just (bhash, bnext))) scroogePK = verify scroogePK (assembleBlockSigData bid bdata (Just bhash)) bsig && verifyBlockChain bnext scroogePK

--
-- Utility stuff
--

assembleBlockSigData :: Int -> BlockData -> Maybe B.ByteString -> B.ByteString
assembleBlockSigData bid bdata bhash = B.concat [sBid, "-", sData, "-", sHash]
    where
    sBid = B.pack $ show bid
    sData = B.pack $ show bdata
    sHash = maybe "" id bhash
