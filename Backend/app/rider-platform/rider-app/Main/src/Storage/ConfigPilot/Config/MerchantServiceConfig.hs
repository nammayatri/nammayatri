{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.MerchantServiceConfig
  ( MerchantServiceConfigDimensions (..),
  )
where

import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Extra.MerchantServiceConfig ()
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types
import qualified Storage.Queries.MerchantServiceConfig as SQMSC
import qualified Storage.Queries.Transformers.MerchantServiceConfig as TRMSC
import Tools.Error

data MerchantServiceConfigDimensions = MerchantServiceConfigDimensions
  { merchantOperatingCityId :: Text,
    merchantId :: Text,
    serviceName :: Maybe DMSC.ServiceName
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

deriving instance Show (DMSC.MerchantServiceConfigD 'Safe)

deriving instance Show (DMSC.MerchantServiceConfigD 'Unsafe)

measureLatency :: MonadFlow m => m a -> Text -> m a
measureLatency action label = do
  startTime <- getCurrentTime
  result <- withLogTag label action
  endTime <- getCurrentTime
  let latency = diffUTCTime endTime startTime
  logDebug $ label <> " Latency: " <> show latency <> " seconds"
  return result

instance ConfigTypeInfo 'MerchantServiceConfig where
  type DimensionsFor 'MerchantServiceConfig = MerchantServiceConfigDimensions
  configTypeValue = MerchantServiceConfig
  sConfigType = SMerchantServiceConfig

instance ConfigDimensions MerchantServiceConfigDimensions where
  type ConfigTypeOf MerchantServiceConfigDimensions = 'MerchantServiceConfig
  type ConfigValueTypeOf MerchantServiceConfigDimensions = [DMSC.MerchantServiceConfig]
  getConfigType _ = MerchantServiceConfig
  getConfigList a = do
    let mocId = a.merchantOperatingCityId
    let mId = a.merchantId
    cfgs <- IM.withInMemCache (configPilotInMemKey MerchantServiceConfig mId) 3600 $ SQMSC.findAllByMerchantId (Id mId)
    let configWrappers = map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) cfgs
    mapM (\configWrapper -> getConfigImpl a configWrapper (LYT.RIDER_CONFIG MerchantServiceConfig) (Id mocId)) configWrappers
  filterByDimensions dims cfgs = filter matchesDimsMocId cfgs
    where
      matchesDimsMocId c =
        maybe True (\sn -> fst (TRMSC.getServiceNameConfigJson c.serviceConfig) == sn) dims.serviceName
          && c.merchantOperatingCityId == Id dims.merchantOperatingCityId
  getConfig dims = do
    allCfgs <- measureLatency (getConfigList dims) ("MerchantServiceConfig.getConfigList merchantId=" <> dims.merchantId <> " mocId=" <> dims.merchantOperatingCityId)
    let foundCfg = filterByDimensions dims allCfgs
    if null foundCfg
      then do
        logError $ "MerchantServiceConfig not found for merchantId: " <> dims.merchantId <> " mocId: " <> dims.merchantOperatingCityId <> " serviceName: " <> show dims.serviceName <> ". Falling back to default city."
        merchant <- CQM.findById (Id dims.merchantId) >>= fromMaybeM (MerchantNotFound dims.merchantId)
        defaultMoc <- CQMOC.findByMerchantShortIdAndCity merchant.shortId merchant.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show merchant.defaultCity)
        let defaultDims = dims {merchantOperatingCityId = defaultMoc.id.getId}
        pure $ filterByDimensions defaultDims allCfgs
      else pure foundCfg
