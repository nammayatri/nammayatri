{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.MerchantServiceConfig
  ( MerchantServiceConfigDimensions (..),
    filterByService,
  )
where

import qualified Domain.Types.MerchantServiceConfig as DMSC
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.Queries.MerchantServiceConfig as SQMSC
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types
import qualified Storage.Queries.Transformers.MerchantServiceConfig as TRMSC

data MerchantServiceConfigDimensions = MerchantServiceConfigDimensions
  { merchantOperatingCityId :: Text,
    serviceName :: Maybe DMSC.ServiceName
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ConfigTypeInfo 'MerchantServiceConfig where
  type DimensionsFor 'MerchantServiceConfig = MerchantServiceConfigDimensions
  configTypeValue = MerchantServiceConfig
  sConfigType = SMerchantServiceConfig

instance ConfigDimensions MerchantServiceConfigDimensions where
  type ConfigTypeOf MerchantServiceConfigDimensions = 'MerchantServiceConfig
  type ConfigValueTypeOf MerchantServiceConfigDimensions = [DMSC.MerchantServiceConfig]
  getConfigType _ = MerchantServiceConfig
  getConfig a = do
    cfgs <- SQMSC.findAllByMerchantOperatingCityId (Id (merchantOperatingCityId a))
    let configWrapper = LYT.Config {config = cfgs, extraDimensions = Nothing, identifier = 0}
    getConfigImpl a configWrapper (LYT.RIDER_CONFIG MerchantServiceConfig) (Id (merchantOperatingCityId a))

filterByService :: [DMSC.MerchantServiceConfig] -> DMSC.ServiceName -> Maybe DMSC.MerchantServiceConfig
filterByService cfgs serviceName = find (\c -> fst (TRMSC.getServiceNameConfigJson c.serviceConfig) == serviceName) cfgs
