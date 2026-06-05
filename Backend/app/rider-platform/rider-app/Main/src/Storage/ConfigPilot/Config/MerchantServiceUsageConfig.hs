{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..)) where

import qualified Domain.Types.MerchantServiceUsageConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as SQ

data MerchantServiceUsageConfigDimensions = MerchantServiceUsageConfigDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'MerchantServiceUsageConfig where
  type DimensionsFor 'MerchantServiceUsageConfig = MerchantServiceUsageConfigDimensions
  configTypeValue = MerchantServiceUsageConfig
  sConfigType = SMerchantServiceUsageConfig

instance ConfigDimensions MerchantServiceUsageConfigDimensions where
  type ConfigTypeOf MerchantServiceUsageConfigDimensions = 'MerchantServiceUsageConfig
  type ConfigValueTypeOf MerchantServiceUsageConfigDimensions = Maybe DT.MerchantServiceUsageConfig
  getConfigType _ = MerchantServiceUsageConfig
  getConfigList a =
    listToMaybe
      <$> LCP.resolveConfigList
        a
        (LYT.RIDER_CONFIG MerchantServiceUsageConfig)
        (Id a.merchantOperatingCityId)
        (maybeToList <$> SQ.findByMerchantOperatingCityId (Id a.merchantOperatingCityId))
        (([] :: [LCP.DimMatcher MerchantServiceUsageConfigDimensions DT.MerchantServiceUsageConfig]))
        Nothing
