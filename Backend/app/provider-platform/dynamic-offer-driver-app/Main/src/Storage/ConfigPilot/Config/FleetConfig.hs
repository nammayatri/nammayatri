{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.FleetConfig (FleetConfigDimensions (..)) where

import qualified Domain.Types.FleetConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.FleetConfig as QFC

data FleetConfigDimensions = FleetConfigDimensions
  { fleetOwnerId :: Text,
    merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'FleetConfig where
  type DimensionsFor 'FleetConfig = FleetConfigDimensions
  configTypeValue = FleetConfig
  sConfigType = SFleetConfig

instance ConfigDimensions FleetConfigDimensions where
  type ConfigTypeOf FleetConfigDimensions = 'FleetConfig
  type ConfigValueTypeOf FleetConfigDimensions = Maybe DT.FleetConfig
  getConfigType _ = FleetConfig
  configDBFetch a = Just (fetchFleetConfig a)
  getConfigList a =
    listToMaybe
      <$> LCP.resolveConfigList
        a
        (LYT.DRIVER_CONFIG FleetConfig)
        (Id a.merchantOperatingCityId)
        (maybeToList <$> fetchFleetConfig a)
        ([] :: [LCP.DimMatcher FleetConfigDimensions DT.FleetConfig])
        Nothing
  configFallback a = Just (fetchFleetConfig a)

fetchFleetConfig :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FleetConfigDimensions -> m (Maybe DT.FleetConfig)
fetchFleetConfig a = QFC.findByFleetOwnerIdAndMerchantOperatingCityId (Id a.fleetOwnerId) (Just (Id a.merchantOperatingCityId))
