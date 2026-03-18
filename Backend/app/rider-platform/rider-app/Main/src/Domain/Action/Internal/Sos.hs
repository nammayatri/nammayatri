module Domain.Action.Internal.Sos where

import Data.Maybe (listToMaybe)
import Data.OpenApi (ToSchema)
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Environment
import EulerHS.Prelude
import qualified Kernel.External.SOS.ERSS.Auth as ERSSAuth
import qualified Kernel.External.SOS.Interface.Types as SOSInterface
import qualified Kernel.External.SOS.Types as SOS
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)

-- | Request body for ERSS reauth — LTS sends this to get a fresh token.
data ErssReauthReq = ErssReauthReq
  { merchantOperatingCityId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- | Response for ERSS reauth.
data ErssReauthResp = ErssReauthResp
  { accessToken :: Text,
    expiresAt :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- | POST /internal/sos/erss-reauth
-- Called by LTS when its cached ERSS bearer token is stale.
-- Auth: validated upstream via api-key header == locationTrackingServiceKey.
postSosErssReauth :: ErssReauthReq -> Flow ErssReauthResp
postSosErssReauth req = do
  let mocId = Id req.merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  _ <- CQMOC.findById mocId >>= fromMaybeM (MerchantOperatingCityNotFound req.merchantOperatingCityId)
  let mscDims = MerchantServiceConfigDimensions {merchantOperatingCityId = mocId.getId, serviceName = Just (DMSC.SOSService SOS.ERSS)}
  mbMerchantSvcCfg <- listToMaybe <$> getConfig mscDims
  case mbMerchantSvcCfg of
    Nothing ->
      throwError $ InvalidRequest "ERSS service config not found for merchantOperatingCityId"
    Just merchantSvcCfg ->
      case merchantSvcCfg.serviceConfig of
        DMSC.SOSServiceConfig (SOSInterface.ERSSConfig erssCfg) -> do
          token <- ERSSAuth.getERSSToken erssCfg
          let expiresAtEpoch = round (POSIX.utcTimeToPOSIXSeconds token.accessExpiresAt) :: Int
          pure
            ErssReauthResp
              { accessToken = token.accessToken,
                expiresAt = expiresAtEpoch
              }
        _ ->
          throwError $ InvalidRequest "SOS service config is not ERSS for this city"
