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

instance ConfigTypeInfo 'MerchantServiceUsageConfigRider where
  type DimensionsFor 'MerchantServiceUsageConfigRider = MerchantServiceUsageConfigDimensions
  configTypeValue = MerchantServiceUsageConfigRider
  sConfigType = SMerchantServiceUsageConfigRider

instance ConfigDimensions MerchantServiceUsageConfigDimensions where
  type ConfigTypeOf MerchantServiceUsageConfigDimensions = 'MerchantServiceUsageConfigRider
  type ConfigValueTypeOf MerchantServiceUsageConfigDimensions = Maybe DT.MerchantServiceUsageConfig
  getConfigType _ = MerchantServiceUsageConfigRider
  getConfigList a =
    listToMaybe
      <$> LCP.resolveConfigList
        a
        (LYT.RIDER_CONFIG MerchantServiceUsageConfigRider)
        (Id a.merchantOperatingCityId)
        (maybeToList <$> SQ.findByMerchantOperatingCityId (Id a.merchantOperatingCityId))
        (([] :: [LCP.DimMatcher MerchantServiceUsageConfigDimensions DT.MerchantServiceUsageConfig]))
        Nothing
