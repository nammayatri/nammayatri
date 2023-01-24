module Storage.Queries.Merchant.MerchantServiceConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (findById)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Merchant as DOrg
import Domain.Types.Merchant.MerchantServiceConfig (MerchantServiceConfig, ServiceConfig, ServiceName)
import Storage.Tabular.Merchant.MerchantServiceConfig

create :: MerchantServiceConfig -> SqlDB ()
create = Esq.create

findByMerchantIdAndService :: Transactionable m => Id Merchant -> ServiceName -> m (Maybe MerchantServiceConfig)
findByMerchantIdAndService merchId service =
  Esq.findOne $ do
    merchantMapsCfg <- from $ table @MerchantServiceConfigT
    where_ $
      merchantMapsCfg ^. MerchantServiceConfigTId ==. val (toKey merchId)
        &&. merchantMapsCfg ^. MerchantServiceConfigServiceName ==. val service
    return merchantMapsCfg

updateMerchantServiceConfig :: Id Merchant -> ServiceConfig -> SqlDB ()
updateMerchantServiceConfig merchantId serviceConfig = do
  now <- getCurrentTime
  let (serviceName, configJSON) = getServiceNameConfigJSON serviceConfig
  Esq.update $ \tbl -> do
    set
      tbl
      [ MerchantServiceConfigConfigJSON =. val configJSON,
        MerchantServiceConfigUpdatedAt =. val now
      ]
    where_ $
      tbl ^. MerchantServiceConfigTId ==. val (toKey merchantId)
        &&. tbl ^. MerchantServiceConfigServiceName ==. val serviceName
