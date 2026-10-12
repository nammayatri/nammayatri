{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.MerchantConfig (MerchantConfigDimensions (..)) where

import qualified Domain.Types.MerchantConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.MerchantConfig as SQ
import qualified Storage.Queries.MerchantConfig as QMerchantConfig

data MerchantConfigDimensions = MerchantConfigDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'MerchantConfig where
  type DimensionsFor 'MerchantConfig = MerchantConfigDimensions
  configTypeValue = MerchantConfig
  sConfigType = SMerchantConfig

instance ConfigDimensions MerchantConfigDimensions where
  type ConfigTypeOf MerchantConfigDimensions = 'MerchantConfig
  type ConfigValueTypeOf MerchantConfigDimensions = [DT.MerchantConfig]
  getConfigType _ = MerchantConfig
  configDBFetch a = Just (QMerchantConfig.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId) True)
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.RIDER_CONFIG MerchantConfig)
      (Id a.merchantOperatingCityId)
      (QMerchantConfig.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId) True)
      ([] :: [LCP.DimMatcher MerchantConfigDimensions DT.MerchantConfig])
      Nothing
  configFallback a = Just (SQ.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId) (Just []))
