{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.DriverPoolConfig (DriverPoolConfigDimensions (..)) where

import qualified Domain.Types.Common
import qualified Domain.Types.DriverPoolConfig as DT
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Types.SpecialLocation
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as SQ

data DriverPoolConfigDimensions = DriverPoolConfigDimensions
  { merchantOperatingCityId :: Text,
    tripDistance :: Maybe Kernel.Types.Common.Meters,
    area :: Maybe Lib.Types.SpecialLocation.Area,
    vehicleVariant :: Kernel.Prelude.Maybe Domain.Types.Common.ServiceTierType,
    tripCategory :: Maybe Kernel.Prelude.Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'DriverPoolConfig where
  type DimensionsFor 'DriverPoolConfig = DriverPoolConfigDimensions
  configTypeValue = DriverPoolConfig
  sConfigType = SDriverPoolConfig

instance ConfigDimensions DriverPoolConfigDimensions where
  type ConfigTypeOf DriverPoolConfigDimensions = 'DriverPoolConfig
  type ConfigValueTypeOf DriverPoolConfigDimensions = [DT.DriverPoolConfig]
  getConfigType _ = DriverPoolConfig
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG DriverPoolConfig)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOpCityId (Id a.merchantOperatingCityId) (Just []) Nothing)
      [ LCP.DimMatcher (.tripDistance) (Just . (.tripDistance)) (==),
        LCP.DimMatcher (.area) (Just . (.area)) (==),
        LCP.DimMatcher (.vehicleVariant) (.vehicleVariant) (==),
        LCP.DimMatcher (.tripCategory) (Just . (.tripCategory)) (==)
      ]
      Nothing
