{-# LANGUAGE TypeApplications #-}

module OnSubscribeDecryption (decryptionTest) where

import Beckn.Product.OnSubscribe (decryptChallenge)
import Crypto.Cipher.AES (AES256)
import qualified Crypto.Cipher.Types as Cipher
import Crypto.Error (throwCryptoErrorIO)
import Crypto.PubKey.Curve25519 (dh, publicKey, secretKey)
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA
import Data.ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import EulerHS.Prelude
import Test.Tasty
import Test.Tasty.HUnit

registryPrivateKeyB16, registryPublicKeyB64, ourPrivateKeyB16, ourPublicKeyB64 :: Text
registryPrivateKeyB16 = "40B74647ED6759FC24EE54B45CB1C775BE6DBE8A77FBD5646553C48885865677"
registryPublicKeyB64 = "C58+MlZoojw5CUaRnif53DC8/0Rkyzje5lsk1W2beF4="
ourPrivateKeyB16 = "801D25C612F7375EF24AFF0ABADEE2ECE9FEF4BD5E32481152B8523C1013B675"
ourPublicKeyB64 = "/JE0quNijg8AyxnRRNZBV3UpNyM8D47ta8LqU7N+a0o="

encryptChallenge :: Text -> IO Text
encryptChallenge challenge = do
  registryPrivateKey <-
    (BA.convertFromBase BA.Base16 (T.encodeUtf8 registryPrivateKeyB16) :: Either String ByteString)
      & fromEitherM "Couldn't convert registry private key from Base16 for encryptChallenge: "
      >>= throwCryptoErrorIO . secretKey

  ourPublicKey <-
    (BA.convertFromBase BA.Base64 (T.encodeUtf8 ourPublicKeyB64) :: Either String ByteString)
      & fromEitherM "Couldn't convert our public key from Base64 for encryptChallenge: "
      >>= throwCryptoErrorIO . publicKey

  let symmetricKey = dh ourPublicKey registryPrivateKey
  cipher <- throwCryptoErrorIO $ Cipher.cipherInit @AES256 @ByteString (BA.convert symmetricKey)
  let encryptedChallenge = Cipher.ctrCombine cipher Cipher.nullIV (T.encodeUtf8 challenge)
  let encryptedChallengeB64 = Base64.encode encryptedChallenge
  pure $ T.decodeUtf8 encryptedChallengeB64
  where
    fromEitherM err = either (error . (err <>) . T.pack) pure

decryptionTest :: TestTree
decryptionTest =
  testCase "Decryption of a challenge for on_subscribe API" $ do
    let challenge = "cryptography challenge"
    encryptedChallenge <- encryptChallenge challenge
    decryptedChallenge <- decryptChallenge encryptedChallenge ourPrivateKeyB16 registryPublicKeyB64
    decryptedChallenge @?= Right challenge
