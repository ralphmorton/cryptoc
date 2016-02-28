{-# LANGUAGE OverloadedStrings #-}

module Cryptoc.DistCoin.BlockChain(
    generateBlock,
    checkBlock,
    transactionValid,
    hashBlock,
    getCoinInfo,
    getCoinsOwnedBy,
    chainLength,
    chainsEquivalent,
    transactionReward
) where

import Cryptoc.BadCrypto
import Cryptoc.DistCoin.Types
import Cryptoc.DistCoin.Network

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Lens ((&), (.~), at)
import Control.Monad (forever, join)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.List ((\\))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (getStdRandom, randomR)

--
-- Notes:
-- 1. Not implementing an adjustment algo for proof of work difficulty,
--    just choosing a difficulty target that works OK on my machine
-- 2. Mining rewards consist solely of coin creation, which I am also
--    not implementing a halfing algo for
--

diffTarget :: Integer
diffTarget = 4 * (10 ^ 110)

transactionReward :: Double
transactionReward = 0.001

--
-- Generate a block, which roughly comes down to finding a nonce such that:
-- H(nonce|blockdata|nexthash) <= difficulty target
--

generateBlock :: [(Transaction, Sig)] -> Maybe BlockHash -> IO Block
generateBlock tx mparent = iterateWhile (not . nonceValid) $ do
    nonce <- (getStdRandom $ randomR (minBound, maxBound) :: IO Int)
    return $ Block nonce tx mparent

--
-- Check a block, both for nonce validity and for transaction validity
--

checkBlock :: Block -> BlockChain -> Bool
checkBlock block blockChain = nextOK && (nonceValid block) && all (transactionValid blockChain) (bTransactions block)
    where
    mNoOrphans = snd $ removeOrphans blockChain
    nextOK = maybe False (isJust . flip M.lookup mNoOrphans) $ bParent block

--
-- Check that a blocks transactions are valid within the context of a blockchain
--

transactionValid :: BlockChain -> SignedTransaction -> Bool
transactionValid bc (t@(Transaction sender input outputs), sig) = sigOK && ownerOK && balOK
    where
    msg = BC.pack $ show t
    sigOK = verify sender msg sig
    coinInfo = fromMaybe (sender, 0) $ getCoinInfo bc input
    ownerOK = fst coinInfo == sender
    outputValue = sum $ fmap snd outputs
    balOK = abs (outputValue - snd coinInfo) < 0.0001

--
-- Check that a block nonce is valid
--

nonceValid :: Block -> Bool
nonceValid block = hashVal <= diffTarget
    where
    hashVal = toNumericVal . hash . BC.pack . show $ block

toNumericVal :: B.ByteString -> Integer
toNumericVal = foldl (\p v -> (p * 256) + fromIntegral v) 0 . B.unpack

--
-- Hash a block
--

hashBlock :: Block -> B.ByteString
hashBlock = hash . BC.pack . show

--
-- Get info about a coin
--

getCoinInfo :: BlockChain -> CoinAddress -> Maybe (PublicKey, Double)
getCoinInfo chain (hash, tix, oix) = do
    let bcNoOrphans = removeOrphans chain
    block <- M.lookup hash $ snd bcNoOrphans
    (trans, _) <- bTransactions block !!? tix
    tOutputCoins trans !!? oix

(!!?) :: [a] -> Int -> Maybe a
(!!?) l i
    | length l <= i = Nothing
    | otherwise = Just $ l !! i

infixl 9 !!?

--
-- Get coins owned by  a public key
--

getCoinsOwnedBy :: BlockChain -> PublicKey -> [(CoinAddress, Double)]
getCoinsOwnedBy chain pk = filter (not . flip any coinsSpent . (==) . fst) allCoins
    where
    bcNoOrphans = removeOrphans chain
    blockChainTransations = fmap (\(hash, block) -> (hash, fmap fst $ bTransactions block)) $ M.toList (snd bcNoOrphans)
    allTransactions = join . fmap snd $ blockChainTransations
    allCoins = foldl (addCoinsFor pk) [] blockChainTransations
    coinsSpent = fmap tInputCoin . filter ((== pk) . tSender) $ allTransactions

addCoinsFor :: PublicKey -> [(CoinAddress, Double)] -> (BlockHash, [Transaction]) -> [(CoinAddress, Double)]
addCoinsFor pk cx (hash, tx) = cx ++ nx
    where nx = join . fmap (toOutputsFor pk hash) . zip [0..] $ tx

toOutputsFor :: PublicKey -> BlockHash -> (Int, Transaction) -> [(CoinAddress, Double)]
toOutputsFor pk hash (tix, Transaction _ _ outputs) = fmap toOutput . filter ((== pk) . fst . snd) . zip [0..] $ outputs
    where
    toOutput (oix, (_, val)) = ((hash, tix, oix), val)

--
-- Remove orphan blocks from a blockchain
--

removeOrphans :: BlockChain -> BlockChain
removeOrphans (h, m) = maybe (h , M.empty) id $ do
    hblock <- M.lookup h m
    let m' = chainFrom h hblock
    return (h, M.fromList m')
    where
    chainFrom h b = maybe [(h, b)] (((h, b):) . uncurry chainFrom) $ do
        h' <- bParent b
        b' <- M.lookup h' m
        return (h', b')

--
-- Calculate chain length
--

chainLength :: BlockChain -> Int
chainLength (h, bc) = maybe 0 len $ M.lookup h bc
    where
    len block = 1 + maybe 0 len (bParent block >>= flip M.lookup bc)

--
-- Check equivalence between chains
--

chainsEquivalent :: BlockChain -> BlockChain -> Bool
chainsEquivalent c1 c2 = removeOrphans c1 == removeOrphans c2

--
-- Utility
--

iterateWhile :: Monad m => (a -> Bool) -> m a -> m a
iterateWhile pred act = do
    r <- act
    case pred r of
        True -> iterateWhile pred act
        False -> return r
