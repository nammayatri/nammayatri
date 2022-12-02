module Storage.Queries.Merchant.MerchantServiceUsageConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (findById)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Merchant as DOrg
import Domain.Types.Merchant.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
import Storage.Tabular.Merchant.MerchantServiceUsageConfig

findByMerchantId :: Transactionable m => Id Merchant -> m (Maybe MerchantServiceUsageConfig)
findByMerchantId merchId =
  Esq.findOne $ do
    merchantMapsCfg <- from $ table @MerchantServiceUsageConfigT
    where_ $
      merchantMapsCfg ^. MerchantServiceUsageConfigTId ==. val (toKey merchId)
    return merchantMapsCfg
