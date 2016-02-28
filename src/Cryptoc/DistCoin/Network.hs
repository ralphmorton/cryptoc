
module Cryptoc.DistCoin.Network(
    createNetwork,
    createTransaction,
    broadcastTransaction,
    broadcastBlock
) where

import Cryptoc.BadCrypto
import Cryptoc.DistCoin.Types

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Lens ((&), (.~), at)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

--
-- Create a network
--

createNetwork :: IO Network
createNetwork = atomically $ (return . Network =<< newTVar M.empty)

--
-- Create a transaction
--

createTransaction :: PrivateKey -> PublicKey -> CoinAddress -> [(PublicKey, Double)] -> IO SignedTransaction
createTransaction sk pk caddr outputs = do
    let trans = Transaction pk caddr outputs
    (Just sig) <- sign sk (B.pack . show $ trans)
    return (trans, sig)


--
-- Broadcast a transaction across the network
--

broadcastTransaction :: Network -> SignedTransaction -> IO ()
broadcastTransaction (Network netNodes) trans = atomically $ do
    nqs <- fmap (fmap snd . M.toList) $ readTVar netNodes
    mapM_ (flip writeTQueue trans) $ fmap snd nqs

--
-- Broadcast a block across the network
--

broadcastBlock :: Network -> Block -> IO ()
broadcastBlock (Network netNodes) block = atomically $ do
    nqs <- fmap (fmap snd . M.toList) $ readTVar netNodes
    mapM_ (flip writeTQueue block) $ fmap fst nqs
