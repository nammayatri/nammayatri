{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.Beckn.FRFS.GWLink where

import Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Extra.MerchantServiceConfig as DMSC
import Domain.Types.FRFSTicket as FRFS
import Domain.Types.Merchant as M
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified EulerHS.Types as Euler
import Kernel.External.Encryption
import Kernel.Prelude hiding (exp)
import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.CacheFlow
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.JWT
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Tools.Error
import Utils.Common.JWT.Config as C
import qualified Utils.Common.JWT.Config as GW
import Utils.Common.JWT.TransitClaim as TC

decodeSA :: T.Text -> Maybe TC.ServiceAccount
decodeSA = J.decode . B.fromStrict . TE.encodeUtf8

getserviceAccount :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id M.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> m TC.ServiceAccount
getserviceAccount merchantId merchantOpCityId serviceName = do
  merchantServiceConfig <- CQMSC.findByMerchantOpCityIdAndService merchantId merchantOpCityId serviceName >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "WalletService" (show GW.GoogleWallet))
  wsc <- case merchantServiceConfig.serviceConfig of
    DEMSC.WalletServiceConfig wsc' -> pure wsc'
    _ -> throwError $ InternalError $ "Unknown Service Config" <> " MerchantOperatingCityId :-" <> merchantOpCityId.getId
  cfg <- case wsc of
    C.GoogleWalletConfig cfg' -> pure cfg'
  saText <- decrypt cfg.walletServiceAccount
  let mSaText = decodeSA saText
  case mSaText of
    Just a -> return a
    Nothing -> throwError $ InternalError $ "Unable to parse Service Account" <> " MerchantOperatingCityId :-" <> merchantOpCityId.getId

type WalletAPI =
  "walletobjects"
    :> "v1"
    :> "transitObject"
    :> Capture "resourceId" Text
    :> Header "Authorization" Text
    :> ReqBody '[JSON] TC.TransitObjectPatch
    :> Patch '[JSON] NoContent

instance ToJSON NoContent where
  toJSON _ = J.Null

updateTicketStatusForGoogleWallet :: (Metrics.CoreMetrics m, MonadFlow m) => TC.TransitObjectPatch -> TC.ServiceAccount -> Text -> m ()
updateTicketStatusForGoogleWallet obj sa resourceId = do
  token' <- liftIO $ TC.getJwtToken sa [("scope", String "https://www.googleapis.com/auth/wallet_object.issuer")]
  token <- fromEitherM (\err -> InternalError $ "Failed to get jwt token" <> show err) token'
  let tokenText = jwtTokenType token <> " " <> jwtAccessToken token
  let eulerClient = Euler.client (Proxy @WalletAPI) resourceId (Just tokenText) obj
  url <- parseBaseUrl "https://walletobjects.googleapis.com"
  void $ callAPI url eulerClient "Calling Google Wallet API" (Proxy @WalletAPI) >>= fromEitherM (FailedToCallWalletAPI . show)

mapToGoogleTicketStatus :: FRFS.FRFSTicketStatus -> Text
mapToGoogleTicketStatus status = newStatus
  where
    newStatus = case status of
      ACTIVE -> "ACTIVE"
      EXPIRED -> "EXPIRED"
      USED -> "COMPLETED"
      CANCELLED -> "INACTIVE"
      COUNTER_CANCELLED -> "INACTIVE"
      CANCEL_INITIATED -> "INACTIVE"
      TECHNICAL_CANCEL_REJECTED -> "INACTIVE"
