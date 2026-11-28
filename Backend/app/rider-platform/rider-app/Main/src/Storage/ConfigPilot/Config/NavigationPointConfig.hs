{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.NavigationPointConfig (NavigationPointConfigDimensions (..)) where

import qualified Domain.Types.NavigationPointConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.NavigationPointConfig as QNavigationPointConfig

newtype NavigationPointConfigDimensions = NavigationPointConfigDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'NavigationPointConfig where
  type DimensionsFor 'NavigationPointConfig = NavigationPointConfigDimensions
  configTypeValue = NavigationPointConfig
  sConfigType = SNavigationPointConfig

instance ConfigDimensions NavigationPointConfigDimensions where
  type ConfigTypeOf NavigationPointConfigDimensions = 'NavigationPointConfig
  type ConfigValueTypeOf NavigationPointConfigDimensions = Maybe DT.NavigationPointConfig
  getConfigType _ = NavigationPointConfig
  configDBFetch a = Just (QNavigationPointConfig.findByMerchantOperatingCityId (Id a.merchantOperatingCityId))
  getConfigList a =
    listToMaybe
      <$> LCP.resolveConfigList
        a
        (LYT.RIDER_CONFIG NavigationPointConfig)
        (Id a.merchantOperatingCityId)
        (maybeToList <$> QNavigationPointConfig.findByMerchantOperatingCityId (Id a.merchantOperatingCityId))
        ([] :: [LCP.DimMatcher NavigationPointConfigDimensions DT.NavigationPointConfig])
        Nothing
  configFallback a = Just (QNavigationPointConfig.findByMerchantOperatingCityId (Id a.merchantOperatingCityId))
