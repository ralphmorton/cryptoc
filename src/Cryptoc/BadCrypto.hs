
module Cryptoc.BadCrypto(
    PublicKey,
    PrivateKey,
    Message,
    Sig,
    genKeyPair,
    sign,
    verify,
    hash
) where

import Crypto.Hash.Algorithms (SHA384)
import qualified Crypto.Hash.SHA384 as SHA2
import Crypto.Random.Types
import Crypto.PubKey.RSA
import qualified Crypto.PubKey.RSA.PKCS15 as PK

import qualified Data.ByteString as B

type Message = B.ByteString
type Sig = B.ByteString

genKeyPair :: MonadRandom m => m (PublicKey, PrivateKey)
genKeyPair = generate 64 65517

sign :: MonadRandom m => PrivateKey -> Message -> m (Maybe Sig)
sign sk m = do
    res <- PK.signSafer (Nothing :: Maybe SHA384) sk $ hash m
    case res of
        Left _ -> return Nothing
        Right sig -> return $ Just sig

verify :: PublicKey -> Message -> Sig -> Bool
verify pk m sig = PK.verify (Nothing :: Maybe SHA384) pk (hash m) sig

hash :: B.ByteString -> B.ByteString
hash = SHA2.hash
