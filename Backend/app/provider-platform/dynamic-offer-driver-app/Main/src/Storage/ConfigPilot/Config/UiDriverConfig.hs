{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.UiDriverConfig (UiDriverConfigDimensions (..)) where

import qualified Domain.Types.UiDriverConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.UiDriverConfig as SQ

data UiDriverConfigDimensions = UiDriverConfigDimensions
  { merchantOperatingCityId :: Text,
    os :: Maybe Kernel.Types.Version.DeviceType,
    platform :: Maybe Lib.Yudhishthira.Types.PlatformType
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'UiDriverConfig where
  type DimensionsFor 'UiDriverConfig = UiDriverConfigDimensions
  configTypeValue = UiDriverConfig
  sConfigType = SUiDriverConfig

instance ConfigDimensions UiDriverConfigDimensions where
  type ConfigTypeOf UiDriverConfigDimensions = 'UiDriverConfig
  type ConfigValueTypeOf UiDriverConfigDimensions = [DT.UiDriverConfig]
  getConfigType _ = UiDriverConfig
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG UiDriverConfig)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOpCityId (Id a.merchantOperatingCityId))
      [ LCP.DimMatcher (.os) (Just . (.os)) (==),
        LCP.DimMatcher (.platform) (Just . (.platform)) (==)
      ]
      Nothing
