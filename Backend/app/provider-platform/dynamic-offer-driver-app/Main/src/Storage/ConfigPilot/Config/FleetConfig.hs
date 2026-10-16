{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.FleetConfig
  ( FleetConfigDimensions (..),
    getCityFleetConfigs,
    fleetForbidsDriverCancellation,
  )
where

import qualified Data.Map.Strict as Map
import qualified Domain.Types.FleetConfig as DT
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.FleetConfig as QFC

-- | All fleet configs of a city, optionally narrowed to one fleet owner.
data FleetConfigDimensions = FleetConfigDimensions
  { merchantOperatingCityId :: Text,
    fleetOwnerId :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'FleetConfig where
  type DimensionsFor 'FleetConfig = FleetConfigDimensions
  configTypeValue = FleetConfig
  sConfigType = SFleetConfig

instance ConfigDimensions FleetConfigDimensions where
  type ConfigTypeOf FleetConfigDimensions = 'FleetConfig
  type ConfigValueTypeOf FleetConfigDimensions = [DT.FleetConfig]
  getConfigType _ = FleetConfig
  configDBFetch a = Just (fetchFleetConfigs a)
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG FleetConfig)
      (Id a.merchantOperatingCityId)
      (QFC.findAllByMerchantOperatingCityId (Just (Id a.merchantOperatingCityId)))
      [LCP.DimMatcher (.fleetOwnerId) (Just . (.fleetOwnerId.getId)) (==)]
      Nothing
  configFallback a = Just (fetchFleetConfigs a)

fetchFleetConfigs :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FleetConfigDimensions -> m [DT.FleetConfig]
fetchFleetConfigs a =
  filter (\cfg -> maybe True (== cfg.fleetOwnerId.getId) a.fleetOwnerId)
    <$> QFC.findAllByMerchantOperatingCityId (Just (Id a.merchantOperatingCityId))

-- | Every fleet's config in the city, keyed by fleet owner id, from one cached ConfigPilot read.
getCityFleetConfigs :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> m (Map.Map Text DT.FleetConfig)
getCityFleetConfigs merchantOpCityId = do
  fleetConfigs <- getConfig (FleetConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, fleetOwnerId = Nothing}) Nothing
  pure $ Map.fromList [(fleetConfig.fleetOwnerId.getId, fleetConfig) | fleetConfig <- fleetConfigs]

-- | A fleet without a config row, or with the flag on, lets its drivers cancel.
fleetForbidsDriverCancellation :: Maybe DT.FleetConfig -> Bool
fleetForbidsDriverCancellation = maybe False (not . (.allowCancellationByDriver))
