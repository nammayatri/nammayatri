{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.VehicleConfig
  ( VehicleConfigDimensions (..),
  )
where

import qualified Domain.Types.VehicleConfig as DVC
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.VehicleConfig as QVC

data VehicleConfigDimensions = VehicleConfigDimensions
  { merchantOperatingCityId :: Text,
    becknConfigId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'VehicleConfig where
  type DimensionsFor 'VehicleConfig = VehicleConfigDimensions
  configTypeValue = VehicleConfig
  sConfigType = SVehicleConfig

instance ConfigDimensions VehicleConfigDimensions where
  type ConfigTypeOf VehicleConfigDimensions = 'VehicleConfig
  type ConfigValueTypeOf VehicleConfigDimensions = [DVC.VehicleConfig]
  getConfigType _ = VehicleConfig
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.RIDER_CONFIG VehicleConfig)
      (Id a.merchantOperatingCityId)
      (QVC.findAllByBecknConfigId (Id a.becknConfigId))
      ([] :: [LCP.DimMatcher VehicleConfigDimensions DVC.VehicleConfig])
      Nothing
  configFallback a = Just (QVC.findAllByBecknConfigId (Id a.becknConfigId))
