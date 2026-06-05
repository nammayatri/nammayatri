{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.GoHomeConfig (GoHomeConfigDimensions (..)) where

import qualified Domain.Types.GoHomeConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.GoHomeConfig as SQ

data GoHomeConfigDimensions = GoHomeConfigDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'GoHomeConfig where
  type DimensionsFor 'GoHomeConfig = GoHomeConfigDimensions
  configTypeValue = GoHomeConfig
  sConfigType = SGoHomeConfig

instance ConfigDimensions GoHomeConfigDimensions where
  type ConfigTypeOf GoHomeConfigDimensions = 'GoHomeConfig
  type ConfigValueTypeOf GoHomeConfigDimensions = Maybe DT.GoHomeConfig
  getConfigType _ = GoHomeConfig
  getConfigList a =
    listToMaybe
      <$> LCP.resolveConfigList
        a
        (LYT.DRIVER_CONFIG GoHomeConfig)
        (Id a.merchantOperatingCityId)
        (maybeToList <$> SQ.getGoHomeConfigFromDB (Id a.merchantOperatingCityId))
        (([] :: [LCP.DimMatcher GoHomeConfigDimensions DT.GoHomeConfig]))
        Nothing
