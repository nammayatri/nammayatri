{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.MerchantServiceConfig
  ( MerchantServiceConfigDimensions (..),
  )
where

import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Extra.MerchantServiceConfig ()
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (incrementConfigPilotFailureCounter, incrementConfigPilotSuccessCounter)
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
import System.Environment (lookupEnv)
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

  -- Bespoke override that ADDS latency measurement + default-city fallback on top of the
  -- library's standard read guarantees. We replicate the lib default's DISABLE_CONFIG_PILOT
  -- kill-switch, fail-open-to-@mbFallback@, and success/failure metrics here (Haskell type
  -- classes have no way to call the shadowed default from an override).
  getConfig dims mbFallback = do
    let tableName = show (getConfigType dims)
    disableConfigPilot <- liftIO $ maybe False read <$> lookupEnv "DISABLE_CONFIG_PILOT"
    withLogTag "CONFIG_PILOT:" $
      if disableConfigPilot
        then fromMaybe (throwM $ InternalError $ "No Fallback configured for table: " <> tableName) mbFallback
        else do
          logDebug $ "getConfig:entry table=" <> tableName <> " dims=" <> show dims
          let onSuccess cfg = do
                incrementConfigPilotSuccessCounter tableName
                pure cfg
              onFailure (e :: SomeException) = do
                logError $ "Fetch failed from config pilot, triggering fallback. error=" <> show e
                incrementConfigPilotFailureCounter tableName
                fromMaybe (throwM e) mbFallback
          result <-
            handle onFailure $
              (>>= onSuccess) $ do
                foundCfg <- measureLatency (getConfigList dims) ("MerchantServiceConfig.getConfigList merchantId=" <> dims.merchantId <> " mocId=" <> dims.merchantOperatingCityId)
                if null foundCfg
                  then do
                    logError $ "MerchantServiceConfig not found for merchantId: " <> dims.merchantId <> " mocId: " <> dims.merchantOperatingCityId <> " serviceName: " <> show dims.serviceName <> ". Falling back to default city."
                    merchant <- CQM.findById (Id dims.merchantId) >>= fromMaybeM (MerchantNotFound dims.merchantId)
                    defaultMoc <- CQMOC.findByMerchantShortIdAndCity merchant.shortId merchant.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show merchant.defaultCity)
                    let defaultDims = dims {merchantOperatingCityId = defaultMoc.id.getId}
                    getConfigList defaultDims
                  else pure foundCfg
          logDebug $ "getConfig:exit table=" <> tableName <> " dims=" <> show dims
          pure result
