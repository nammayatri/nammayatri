{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.Beckn.FRFS.GWLink where

import Data.Aeson as J
import Data.Text as T
import Data.Time hiding (getCurrentTime)
import Data.Time.Clock.POSIX
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Extra.MerchantServiceConfig as DMSC
import qualified Domain.Types.FRFSTicketStatus as FRFS
import Domain.Types.Merchant as M
import qualified Domain.Types.MerchantOperatingCity as DMOC
import EulerHS.Prelude hiding (exp)
import qualified EulerHS.Types as Euler
import Kernel.External.Encryption
import Kernel.Prelude hiding (exp)
import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.CacheFlow
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.JWT hiding (ServiceAccount (..))
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Tools.Error
import Utils.Common.JWT.Config as C
import qualified Utils.Common.JWT.Config as GW
import Utils.Common.JWT.TransitClaim as TC
import Web.JWT hiding (claims)

data GoogleWalletTicketState
  = ACTIVE
  | EXPIRED
  | COMPLETED
  | INACTIVE
  deriving stock (Eq, Show, Generic)

data GoogleQRCode = QR_CODE deriving stock (Eq, Show, Generic)

data GoogleTripType = ONE_WAY | ROUND_TRIP deriving stock (Eq, Show, Generic)

data GooglePassengerType = SINGLE_PASSENGER | MULTIPLE_PASSENGERS deriving stock (Eq, Show, Generic)

getCustomCardTitleValueByTripType :: GoogleTripType -> Text
getCustomCardTitleValueByTripType tripType =
  case tripType of
    ONE_WAY -> "Single ticket"
    ROUND_TRIP -> "Round trip"

getserviceAccount :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id M.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> m TC.ServiceAccount
getserviceAccount merchantId merchantOpCityId serviceName = do
  merchantServiceConfig <- CQMSC.findByMerchantOpCityIdAndService merchantId merchantOpCityId serviceName >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "WalletService" (show GW.GoogleWallet))
  wsc <- case merchantServiceConfig.serviceConfig of
    DEMSC.WalletServiceConfig wsc' -> pure wsc'
    _ -> throwError $ InternalError $ "Unknown Service Config" <> " MerchantOperatingCityId :-" <> merchantOpCityId.getId
  cfg <- case wsc of
    C.GoogleWalletConfig cfg' -> pure cfg'
  privateKeyId <- decrypt cfg.privateKeyId
  issuerId <- decrypt cfg.issuerId
  return
    TC.ServiceAccount
      { TC.saPrivateKeyId = privateKeyId,
        TC.saClientEmail = cfg.clientEmail,
        TC.saTokenUri = cfg.tokenUri,
        TC.saIssuerId = issuerId
      }

type PatchWalletAPI =
  "walletobjects"
    :> "v1"
    :> "transitObject"
    :> Capture "resourceId" Text
    :> Header "Authorization" Text
    :> ReqBody '[JSON] TC.TransitObjectPatch
    :> Patch '[JSON] NoContent

instance ToJSON NoContent where
  toJSON _ = J.Null

type GetWalletAPI =
  "walletobjects"
    :> "v1"
    :> "transitObject"
    :> Capture "resourceId" Text
    :> Header "Authorization" Text
    :> Get '[JSON] NoContent

updateTicketStatusForGoogleWallet :: (Metrics.CoreMetrics m, MonadFlow m, HasFlowEnv m r '["googleSAPrivateKey" ::: String], HasRequestId r, MonadReader r m) => TC.TransitObjectPatch -> TC.ServiceAccount -> Text -> m ()
updateTicketStatusForGoogleWallet obj sa resourceId = do
  privateKey <- asks (.googleSAPrivateKey)
  let additionalClaims = TC.createAdditionalClaims [("scope", String "https://www.googleapis.com/auth/wallet_object.issuer")]
  let jwtHeader =
        JOSEHeader
          { typ = Just "JWT",
            cty = Nothing,
            alg = Just RS256,
            kid = Just $ saPrivateKeyId sa
          }
  let iss = stringOrURI . saClientEmail $ sa
  let aud = Left <$> (stringOrURI . saTokenUri $ sa)
  let now = liftIO $ getPOSIXTime
  iat <- numericDate <$> now
  exp <- numericDate . (+ 3600) <$> now
  let claims =
        mempty
          { exp = exp,
            iat = iat,
            iss = iss,
            aud = aud,
            unregisteredClaims = additionalClaims
          }
  token' <- liftIO $ TC.getJwtToken jwtHeader claims sa privateKey
  token <- fromEitherM (\err -> InternalError $ "Failed to get jwt token" <> show err) token'
  let tokenText = jwtTokenType token <> " " <> jwtAccessToken token
  let eulerClient = Euler.client (Proxy @PatchWalletAPI) resourceId (Just tokenText) obj
  url <- parseBaseUrl "https://walletobjects.googleapis.com"
  void $ callAPI url eulerClient "Calling Google Wallet API" (Proxy @PatchWalletAPI) >>= fromEitherM (FailedToCallWalletAPI . show)

getObjectGoogleWallet :: (Metrics.CoreMetrics m, MonadFlow m, HasFlowEnv m r '["googleSAPrivateKey" ::: String], HasRequestId r, MonadReader r m) => TC.ServiceAccount -> Text -> m (Maybe NoContent)
getObjectGoogleWallet sa resourceId = do
  privateKey <- asks (.googleSAPrivateKey)
  let additionalClaims = TC.createAdditionalClaims [("scope", String "https://www.googleapis.com/auth/wallet_object.issuer")]
  let jwtHeader =
        JOSEHeader
          { typ = Just "JWT",
            cty = Nothing,
            alg = Just RS256,
            kid = Just $ saPrivateKeyId sa
          }
  let iss = stringOrURI . saClientEmail $ sa
  let aud = Left <$> (stringOrURI . saTokenUri $ sa)
  let now = liftIO $ getPOSIXTime
  iat <- numericDate <$> now
  exp <- numericDate . (+ 3600) <$> now
  let claims =
        mempty
          { exp = exp,
            iat = iat,
            iss = iss,
            aud = aud,
            unregisteredClaims = additionalClaims
          }
  token' <- liftIO $ TC.getJwtToken jwtHeader claims sa privateKey
  token <- fromEitherM (\err -> InternalError $ "Failed to get jwt token" <> show err) token'
  let tokenText = jwtTokenType token <> " " <> jwtAccessToken token
  let eulerClient = Euler.client (Proxy @GetWalletAPI) resourceId (Just tokenText)
  url <- parseBaseUrl "https://walletobjects.googleapis.com"
  callAPI url eulerClient "Calling Google Wallet API" (Proxy @GetWalletAPI) >>= \case
    Left _ -> pure Nothing
    Right _ -> pure $ Just NoContent

mapToGoogleTicketStatus :: FRFS.FRFSTicketStatus -> GoogleWalletTicketState
mapToGoogleTicketStatus status = newStatus
  where
    newStatus = case status of
      FRFS.ACTIVE -> ACTIVE
      FRFS.INPROGRESS -> ACTIVE
      FRFS.EXPIRED -> EXPIRED
      FRFS.USED -> COMPLETED
      FRFS.CANCELLED -> INACTIVE
      FRFS.COUNTER_CANCELLED -> INACTIVE
      FRFS.CANCEL_INITIATED -> INACTIVE
      FRFS.TECHNICAL_CANCEL_REJECTED -> INACTIVE

showTimeIst :: UTCTime -> Text
showTimeIst time =
  T.pack $
    formatTime defaultTimeLocale "%d %b %Y, %I:%M %p" $
      addUTCTime (60 * 330) time

utcTimeToText :: UTCTime -> Text
utcTimeToText = T.pack . formatTime defaultTimeLocale "%FT%TZ"
