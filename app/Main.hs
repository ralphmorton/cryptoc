
module Main where

import Cryptoc.BadCrypto
import Cryptoc.DistCoin

import Control.Concurrent
import Control.Monad
import System.Random

main :: IO ()
main = do
    (_daemonPK, _daemonSK) <- genKeyPair
    (alicePK, aliceSK) <- genKeyPair
    (bobPK, bobSK) <- genKeyPair
    (charliePK, charlieSK) <- genKeyPair
    network <- createNetwork
    node1 <- createGenesisNode network _daemonSK _daemonPK alicePK [(alicePK, 5), (bobPK, 5)]
    threadDelay 1000000
    node2 <- createNode network bobPK node1
    threadDelay 1000000

    bc <- getBlockChain node1
    let (caddr, cval) = head $ getCoinsOwnedBy bc bobPK
    putStrLn $ show (caddr, cval)
    sigTrans <- createTransaction bobSK bobPK caddr [(alicePK, 2.5), (bobPK, cval - 2.5)]
    broadcastTransaction network sigTrans

    forever $ do
        bc1 <- getBlockChain node1
        bc2 <- getBlockChain node2
        putStrLn $ "Alice Balance: " ++ show (calcBalance bc1 alicePK)
        putStrLn $ "Bob Balance: " ++ show (calcBalance bc1 bobPK)
        putStrLn $ "Blockchains equivalent?: " ++ show (chainsEquivalent bc1 bc2)
        threadDelay 10000000

calcBalance :: BlockChain -> PublicKey -> Double
calcBalance bc pk = sum . fmap snd $ getCoinsOwnedBy bc pk

randBool :: IO Bool
randBool = do
    r <- (getStdRandom $ randomR (1,10) :: IO Int)
    return $ r > 5
