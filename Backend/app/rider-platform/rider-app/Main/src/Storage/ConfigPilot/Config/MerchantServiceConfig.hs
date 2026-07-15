{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.MerchantServiceConfig
  ( MerchantServiceConfigDimensions (..),
  )
where

import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Extra.MerchantServiceConfig ()
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.ConfigPilot.Interface.Getter as CR
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
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
  getConfigList a =
    CR.resolveConfigList
      a
      (LYT.RIDER_CONFIG MerchantServiceConfig)
      (Id a.merchantOperatingCityId)
      (SQMSC.findAllByMerchantId (Id a.merchantId))
      [ CR.DimMatcher (\dims -> Just dims.merchantOperatingCityId) (\c -> Just (c.merchantOperatingCityId.getId)) (==),
        CR.DimMatcher (.serviceName) (\c -> Just $ fst (TRMSC.getServiceNameConfigJson c.serviceConfig)) (==)
      ]
      Nothing
  getConfig dims _mbFallback = do
    foundCfg <- measureLatency (getConfigList dims) ("MerchantServiceConfig.getConfigList merchantId=" <> dims.merchantId <> " mocId=" <> dims.merchantOperatingCityId)
    if null foundCfg
      then do
        logError $ "MerchantServiceConfig not found for merchantId: " <> dims.merchantId <> " mocId: " <> dims.merchantOperatingCityId <> " serviceName: " <> show dims.serviceName <> ". Falling back to default city."
        merchant <- CQM.findById (Id dims.merchantId) >>= fromMaybeM (MerchantNotFound dims.merchantId)
        defaultMoc <- CQMOC.findByMerchantShortIdAndCity merchant.shortId merchant.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show merchant.defaultCity)
        let defaultDims = dims {merchantOperatingCityId = defaultMoc.id.getId}
        getConfigList defaultDims
      else pure foundCfg
