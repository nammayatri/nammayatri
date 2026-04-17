{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.MerchantServiceConfig
  ( MerchantServiceConfigDimensions (..),
  )
where

import qualified Domain.Types.MerchantServiceConfig as DMSC
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types
import Storage.Queries.Transformers.MerchantServiceConfig (getServiceName)

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
  getConfigList a = do
    let mocId = a.merchantOperatingCityId
    IM.withInMemCache (configPilotInMemKey a) 3600 $ do
      cfgs <- CQMSC.findAllMerchantOpCityId (Id mocId)
      let filtered = filterByDimensions a cfgs
      let configWrappers = map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) filtered
      mapM (\configWrapper -> getConfigImpl a configWrapper (LYT.DRIVER_CONFIG MerchantServiceConfig) (Id mocId)) configWrappers
  filterByDimensions dims cfgs = filter matchesDimsServiceName cfgs
    where
      matchesDimsServiceName c =
        maybe True (\sn -> getServiceName c.serviceConfig == sn) dims.serviceName
