{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.MerchantServiceUsageConfig
  ( MerchantServiceUsageConfigDimensions (..),
  )
where

import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types

data MerchantServiceUsageConfigDimensions = MerchantServiceUsageConfigDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ConfigTypeInfo 'MerchantServiceUsageConfig where
  type DimensionsFor 'MerchantServiceUsageConfig = MerchantServiceUsageConfigDimensions
  configTypeValue = MerchantServiceUsageConfig
  sConfigType = SMerchantServiceUsageConfig

instance ConfigDimensions MerchantServiceUsageConfigDimensions where
  type ConfigTypeOf MerchantServiceUsageConfigDimensions = 'MerchantServiceUsageConfig
  type ConfigValueTypeOf MerchantServiceUsageConfigDimensions = Maybe DMSUC.MerchantServiceUsageConfig
  getConfigType _ = MerchantServiceUsageConfig
  getConfig a = do
    cfg <- CQMSUC.findByMerchantOperatingCityId (Id (merchantOperatingCityId a))
    let configWrapper = LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}
    getConfigImpl a configWrapper (LYT.RIDER_CONFIG MerchantServiceUsageConfig) (Id (merchantOperatingCityId a))
