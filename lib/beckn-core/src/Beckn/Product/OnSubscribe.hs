{-# LANGUAGE TypeApplications #-}

module Beckn.Product.OnSubscribe where

import Beckn.Types.App (HasFlowEnv, RegistrySecrets)
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Types.Field ((:::))
import qualified Beckn.Types.Registry.API as API
import Beckn.Utils.Error (fromEitherM)
import Crypto.Cipher.AES (AES256)
import qualified Crypto.Cipher.Types as Cipher
import Crypto.Error (throwCryptoErrorIO)
import qualified Crypto.PubKey.Curve25519 as Curve25519
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import EulerHS.Language (runIO)
import EulerHS.Prelude

onSubscribe ::
  HasFlowEnv m r '["registrySecrets" ::: RegistrySecrets] =>
  Text ->
  API.OnSubscribeRequest ->
  m API.OnSubscribeResponse
onSubscribe registryEncryptionPubKey req = do
  registrySecrets <- asks (.registrySecrets)
  let ourEncryptionPrivKey = registrySecrets.encryptionPrivKeyB16
  eitherDecryptedChallenge <- runIO $ decryptChallenge req.challenge ourEncryptionPrivKey registryEncryptionPubKey
  decryptedChallenge <- eitherDecryptedChallenge & fromEitherM (InternalError . T.pack)
  pure $ API.OnSubscribeResponse decryptedChallenge

decryptChallenge :: Text -> Text -> Text -> IO (Either String Text)
decryptChallenge challengeB64 ourPrivKeyB16 registryPubKeyB64 = do
  let decodedData = (,,) <$> parseChallenge <*> parsePrivateKey <*> parsePublicKey
  case decodedData of
    Left err -> pure $ Left err
    Right (challenge, ourPrivKey, registryPubKey) -> do
      privKey <- throwCryptoErrorIO $ Curve25519.secretKey ourPrivKey
      pubKey <- throwCryptoErrorIO $ Curve25519.publicKey registryPubKey
      let dhKey = Curve25519.dh pubKey privKey
      cipher <- throwCryptoErrorIO $ Cipher.cipherInit @AES256 @ByteString (BA.convert dhKey)
      let decryptedChallenge = Cipher.ctrCombine cipher Cipher.nullIV challenge
      pure . Right $ T.decodeUtf8 decryptedChallenge
  where
    parseChallenge, parsePrivateKey, parsePublicKey :: Either String ByteString
    parseChallenge =
      first ("Unable to convert challenge from Base64: " ++) $
        BA.convertFromBase BA.Base64 $ T.encodeUtf8 challengeB64
    parsePrivateKey =
      first ("Unable to convert private key: " ++) $
        BA.convertFromBase BA.Base16 $ T.encodeUtf8 ourPrivKeyB16
    parsePublicKey =
      first ("Unable to convert public key: " ++) $
        BA.convertFromBase BA.Base64 $ T.encodeUtf8 registryPubKeyB64
