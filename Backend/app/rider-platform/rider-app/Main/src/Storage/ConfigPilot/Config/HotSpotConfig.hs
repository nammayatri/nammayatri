{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.HotSpotConfig (HotSpotConfigDimensions (..)) where

import qualified Domain.Types.HotSpotConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.HotSpotConfig as SQ

data HotSpotConfigDimensions = HotSpotConfigDimensions
  { merchantOperatingCityId :: Text, -- is used only for dynamic logic rollout, can be left as "" for this config.
    merchantId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'HotSpotConfig where
  type DimensionsFor 'HotSpotConfig = HotSpotConfigDimensions
  configTypeValue = HotSpotConfig
  sConfigType = SHotSpotConfig

instance ConfigDimensions HotSpotConfigDimensions where
  type ConfigTypeOf HotSpotConfigDimensions = 'HotSpotConfig
  type ConfigValueTypeOf HotSpotConfigDimensions = Maybe DT.HotSpotConfig
  getConfigType _ = HotSpotConfig
  getConfigList a =
    listToMaybe
      <$> LCP.resolveConfigList
        a
        (LYT.RIDER_CONFIG HotSpotConfig)
        (Id a.merchantOperatingCityId)
        (maybeToList <$> SQ.findConfigByMerchantId (Id a.merchantId))
        (([] :: [LCP.DimMatcher HotSpotConfigDimensions DT.HotSpotConfig]))
        Nothing
  configFallback a = Just (SQ.findConfigByMerchantId (Id a.merchantId))
