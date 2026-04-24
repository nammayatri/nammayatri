module Domain.Action.UI.SVP
  ( SignQRReq (..),
    SignQRResp (..),
    RotateKeysResp (..),
    signQR,
    getPublicKey,
    rotateKeys,
  )
where

import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA.PKCS15
import qualified Crypto.Store.PKCS8 as PKCS8
import qualified Crypto.Store.X509 as StoreX509
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.X509 (PrivKey (..), PubKey (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import Tools.Error

rsaPrivateKeyRedisKey :: Text
rsaPrivateKeyRedisKey = "SVP:RSAPrivateKeyPEM"

data SignQRReq = SignQRReq
  { plaintext :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data SignQRResp = SignQRResp
  { signature :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data RotateKeysResp = RotateKeysResp
  { rotatedAt :: UTCTime,
    publicKey :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

signQR ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  SignQRReq ->
  m SignQRResp
signQR SignQRReq {..} = do
  when (T.null plaintext) $ throwError $ InvalidRequest "plaintext is required"
  privateKey <- loadOrBootstrapKey
  case RSA.PKCS15.sign Nothing (Just Hash.SHA256) privateKey (TE.encodeUtf8 plaintext) of
    Left err -> do
      logError $ "[SVP:signQR] Signing failed: " <> T.pack (show err)
      throwError $ InternalError "SVP signing failed"
    Right sig ->
      pure $ SignQRResp {signature = TE.decodeUtf8 (B64.encode sig)}

getPublicKey ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  m Text
getPublicKey = do
  privateKey <- loadOrBootstrapKey
  pure $ encodePublicKeyPem (RSA.private_pub privateKey)


rotateKeys ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  m RotateKeysResp
rotateKeys = do
  logInfo "[SVP:rotateKeys] Generating new RSA-2048 key pair"
  (pub, priv) <- liftIO $ RSA.generate 256 0x10001
  Hedis.set rsaPrivateKeyRedisKey (encodePrivateKeyPem priv)
  now <- getCurrentTime
  logInfo $ "[SVP:rotateKeys] Rotation complete at " <> T.pack (show now)
  pure $ RotateKeysResp {rotatedAt = now, publicKey = encodePublicKeyPem pub}

loadOrBootstrapKey ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  m RSA.PrivateKey
loadOrBootstrapKey = do
  mbPem <- Hedis.get rsaPrivateKeyRedisKey
  pem <- case mbPem of
    Just p -> pure p
    Nothing -> do
      logInfo "[SVP] No private key in Redis; bootstrapping fresh RSA-2048 key"
      _ <- rotateKeys
      Hedis.get rsaPrivateKeyRedisKey
        >>= fromMaybeM (InternalError "SVP: key bootstrap failed — Redis read after write returned empty")
  case parsePrivateKeyPem pem of
    Just k -> pure k
    Nothing -> do
      logError "[SVP] Failed to parse RSA private key PEM stored in Redis"
      throwError $ InternalError "SVP: invalid RSA private key PEM"

encodePublicKeyPem :: RSA.PublicKey -> Text
encodePublicKeyPem pub =
  TE.decodeUtf8 $ StoreX509.writePubKeyFileToMemory [PubKeyRSA pub]

encodePrivateKeyPem :: RSA.PrivateKey -> Text
encodePrivateKeyPem priv =
  TE.decodeUtf8 $
    PKCS8.writeKeyFileToMemory PKCS8.PKCS8Format [PrivKeyRSA priv]

parsePrivateKeyPem :: Text -> Maybe RSA.PrivateKey
parsePrivateKeyPem pem =
  let bs = TE.encodeUtf8 pem
   in case PKCS8.readKeyFileFromMemory bs of
        (PKCS8.Unprotected (PrivKeyRSA k) : _) -> Just k
        _ -> Nothing
