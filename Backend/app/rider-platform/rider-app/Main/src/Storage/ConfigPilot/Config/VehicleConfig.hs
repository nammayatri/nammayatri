{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.VehicleConfig
  ( VehicleConfigDimensions (..),
  )
where

import qualified Domain.Types.VehicleConfig as DVC
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types
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
  getConfigList a = do
    IM.withInMemCache (configPilotInMemKey a) 3600 $ do
      cfgs <- QVC.findAllByBecknConfigId (Id a.becknConfigId)
      let configWrappers = map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) cfgs
      mapM (\configWrapper -> getConfigImpl a configWrapper (LYT.RIDER_CONFIG VehicleConfig) (Id a.merchantOperatingCityId)) configWrappers
