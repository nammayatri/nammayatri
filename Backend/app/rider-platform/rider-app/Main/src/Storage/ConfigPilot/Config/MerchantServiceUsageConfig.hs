{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.MerchantServiceUsageConfig
  ( MerchantServiceUsageConfigDimensions (..),
  )
where

import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types

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
  type ConfigValueTypeOf MerchantServiceUsageConfigDimensions = Maybe DMSUC.MerchantServiceUsageConfig
  getConfigType _ = MerchantServiceUsageConfig
  getConfigList a = do
    let mocId = a.merchantOperatingCityId
    IM.withInMemCache (configPilotInMemKey a) 3600 $ do
      cfg <- CQMSUC.findByMerchantOperatingCityId (Id mocId)
      let configWrapper = LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}
      getConfigImpl a configWrapper (LYT.RIDER_CONFIG MerchantServiceUsageConfig) (Id mocId)
