{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..)) where

import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Extra.MerchantServiceConfig ()
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as SQ
import qualified Storage.Queries.Transformers.MerchantServiceConfig as TRMSC

data MerchantServiceConfigDimensions = MerchantServiceConfigDimensions
  { merchantOperatingCityId :: Text,
    merchantId :: Maybe Text,
    serviceName :: Maybe DMSC.ServiceName
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ConfigTypeInfo 'MerchantServiceConfigDriver where
  type DimensionsFor 'MerchantServiceConfigDriver = MerchantServiceConfigDimensions
  configTypeValue = MerchantServiceConfigDriver
  sConfigType = SMerchantServiceConfigDriver

instance ConfigDimensions MerchantServiceConfigDimensions where
  type ConfigTypeOf MerchantServiceConfigDimensions = 'MerchantServiceConfigDriver
  type ConfigValueTypeOf MerchantServiceConfigDimensions = [DMSC.MerchantServiceConfig]
  getConfigType _ = MerchantServiceConfigDriver
  configFallback a = (\svc -> maybeToList <$> SQ.findByServiceAndCity svc (Id a.merchantOperatingCityId)) <$> a.serviceName
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG MerchantServiceConfig)
      (Id a.merchantOperatingCityId)
      (SQ.findAllMerchantOpCityId (Id a.merchantOperatingCityId))
      [ LCP.DimMatcher (.serviceName) (\c -> Just $ TRMSC.getServiceName c.serviceConfig) (==)
      ]
      Nothing
