{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.MerchantConfig
  ( MerchantConfigDimensions (..),
  )
where

import qualified Domain.Types.MerchantConfig as DMC
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.CachedQueries.MerchantConfig as SCMC
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types

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
  type ConfigValueTypeOf MerchantConfigDimensions = [DMC.MerchantConfig]
  getConfigType _ = MerchantConfig
  getConfigList a = do
    let mocId = a.merchantOperatingCityId
    cfgs <- IM.withInMemCache (configPilotInMemKey MerchantConfig mocId) 3600 $ SCMC.findAllByMerchantOperatingCityId (Id mocId) (Just [])
    let configWrappers = map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) cfgs
    mapM (\configWrapper -> getConfigImpl a configWrapper (LYT.RIDER_CONFIG MerchantConfig) (Id mocId)) configWrappers
