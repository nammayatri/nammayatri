{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.DriverPoolConfig (DriverPoolConfigDimensions (..)) where

import qualified Domain.Types.DriverPoolConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as SQ

data DriverPoolConfigDimensions = DriverPoolConfigDimensions
  { merchantOperatingCityId :: Text
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
      ([] :: [LCP.DimMatcher DriverPoolConfigDimensions DT.DriverPoolConfig])
      Nothing
