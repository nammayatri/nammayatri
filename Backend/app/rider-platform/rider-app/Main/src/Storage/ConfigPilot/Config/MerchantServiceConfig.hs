{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.MerchantServiceConfig
  ( MerchantServiceConfigDimensions (..),
  )
where

import qualified Domain.Types.MerchantServiceConfig as DMSC
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types
import qualified Storage.Queries.MerchantServiceConfig as SQMSC
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
  getConfigList a = do
    let mocId = a.merchantOperatingCityId
    cfgs <- SQMSC.findAllByMerchantOperatingCityId (Id mocId)
    let configWrappers = map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) cfgs
    mapM (\configWrapper -> getConfigImpl a configWrapper (LYT.RIDER_CONFIG MerchantServiceConfig) (Id mocId)) configWrappers
  filterByDimensions dims cfgs = filter matchesDims cfgs
    where
      matchesDims c =
        maybe True (\sn -> fst (TRMSC.getServiceNameConfigJson c.serviceConfig) == sn) dims.serviceName
