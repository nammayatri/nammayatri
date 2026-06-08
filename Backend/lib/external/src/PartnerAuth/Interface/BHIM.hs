module PartnerAuth.Interface.BHIM
  ( verifyToken,
    getUserDetails,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified PartnerAuth.BHIM.API as API
import PartnerAuth.BHIM.Config (BhimCfg)
import PartnerAuth.BHIM.Encryption (decryptEnvelope, encryptEnvelope)
import PartnerAuth.BHIM.Types
import qualified PartnerAuth.Types as PT

-- | Verify a BHIM token via the S2S verify-token API. Returns the partner's
-- isValid verdict. A BHIM error body (errorCode) is logged and surfaced as a
-- thrown error (the caller maps any failure to isValid:false).
verifyToken :: (EncFlow m r, CoreMetrics m, Log m, HasRequestId r, MonadReader r m) => BhimCfg -> Text -> m Bool
verifyToken cfg token = do
  key <- decrypt cfg.aesKey
  enc <- encryptEnvelope key (encodeToText (TokenReq token))
  resp <- API.callVerifyToken cfg.baseUrl (Just cfg.partnerId) (EncEnvelope enc)
  decrypted <- decryptOrThrow key resp.encResponseBody "verify-token"
  case A.decode (toLBS decrypted) :: Maybe VerifyTokenInnerRes of
    Just r -> pure r.isValid
    Nothing -> handleErrorBody "verify-token" decrypted

-- | Fetch user details via the S2S user-details API.
getUserDetails :: (EncFlow m r, CoreMetrics m, Log m, HasRequestId r, MonadReader r m) => BhimCfg -> Text -> m PT.PartnerUserDetails
getUserDetails cfg token = do
  key <- decrypt cfg.aesKey
  enc <- encryptEnvelope key (encodeToText (TokenReq token))
  resp <- API.callUserDetails cfg.baseUrl (Just cfg.partnerId) (EncEnvelope enc)
  decrypted <- decryptOrThrow key resp.encResponseBody "user-details"
  case A.decode (toLBS decrypted) :: Maybe UserDetailsInnerRes of
    Just r -> pure $ PT.PartnerUserDetails {name = r.name, mobileNumber = r.mobileNumber}
    Nothing -> handleErrorBody "user-details" decrypted

decryptOrThrow :: (MonadFlow m) => Text -> Text -> Text -> m Text
decryptOrThrow key enc label =
  case decryptEnvelope key enc of
    Left err -> throwError $ InternalError ("BHIM " <> label <> " decrypt failed: " <> T.pack err)
    Right t -> pure t

handleErrorBody :: (MonadFlow m, Log m) => Text -> Text -> m a
handleErrorBody label decrypted = do
  let mbErr = A.decode (toLBS decrypted) :: Maybe BhimErrorBody
  -- Log only safe metadata (endpoint label + error code). Never log the
  -- decrypted body: for user-details it contains PII (name / mobile number).
  logError $ "BHIM " <> label <> " returned error" <> maybe " (unparseable body)" (\e -> ", errorCode=" <> e.errorCode) mbErr
  throwError $ InternalError ("BHIM " <> label <> " failed")

toLBS :: Text -> BL.ByteString
toLBS = BL.fromStrict . TE.encodeUtf8
