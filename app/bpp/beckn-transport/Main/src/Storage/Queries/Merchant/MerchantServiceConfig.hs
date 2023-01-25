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
import Domain.Types.Merchant.MerchantServiceConfig (MerchantServiceConfig, ServiceName)
import Storage.Tabular.Merchant.MerchantServiceConfig

findByMerchantIdAndService :: Transactionable m => Id Merchant -> ServiceName -> m (Maybe MerchantServiceConfig)
findByMerchantIdAndService merchantId serviceName =
  Esq.findOne $ do
    merchantServiceConfig <- from $ table @MerchantServiceConfigT
    where_ $
      merchantServiceConfig ^. MerchantServiceConfigTId ==. val (toKey (merchantId, serviceName))
    return merchantServiceConfig

upsertMerchantServiceConfig :: MerchantServiceConfig -> SqlDB ()
upsertMerchantServiceConfig merchantServiceConfig = do
  now <- getCurrentTime
  let (_serviceName, configJSON) = getServiceNameConfigJSON merchantServiceConfig.serviceConfig
  Esq.upsert
    merchantServiceConfig
    [ MerchantServiceConfigConfigJSON =. val configJSON,
      MerchantServiceConfigUpdatedAt =. val now
    ]
