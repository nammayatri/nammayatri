{-# OPTIONS_GHC -Wno-orphans #-}

-- | Fleet config through Config Pilot: a city's fleet_config rows are loaded once and kept in
-- memory, then filtered by fleet owner. Fleet Features read it on hot paths (pool build, driver
-- cancel, ride lists, driver preference APIs), so these reads must not hit KV/DB each time.
module Storage.ConfigPilot.Config.FleetConfig (FleetConfigDimensions (..)) where

import qualified Domain.Types.FleetConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.FleetConfig as QFC

-- | @fleetOwnerId = Nothing@ returns every fleet config of the city.
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
  configDBFetch a = Just (QFC.findAllByMerchantOperatingCityId (Just (Id a.merchantOperatingCityId)))
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG FleetConfig)
      (Id a.merchantOperatingCityId)
      (QFC.findAllByMerchantOperatingCityId (Just (Id a.merchantOperatingCityId)))
      [LCP.DimMatcher (.fleetOwnerId) (Just . (.fleetOwnerId.getId)) (==)]
      Nothing
  configFallback a = Just $ case a.fleetOwnerId of
    Just fleetOwnerId -> maybeToList <$> QFC.findByPrimaryKey (Id fleetOwnerId)
    Nothing -> QFC.findAllByMerchantOperatingCityId (Just (Id a.merchantOperatingCityId))
