module Storage.Queries.Merchant.MerchantServiceUsageConfig
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
import Domain.Types.Merchant.MerchantServiceUsageConfig
import Storage.Tabular.Merchant.MerchantServiceUsageConfig

findByMerchantId :: Transactionable m => Id Merchant -> m (Maybe MerchantServiceUsageConfig)
findByMerchantId merchId =
  Esq.findOne $ do
    merchantMapsCfg <- from $ table @MerchantServiceUsageConfigT
    where_ $
      merchantMapsCfg ^. MerchantServiceUsageConfigTId ==. val (toKey merchId)
    return merchantMapsCfg

updateMerchantServiceUsageConfig ::
  MerchantServiceUsageConfig ->
  SqlDB ()
updateMerchantServiceUsageConfig MerchantServiceUsageConfig {..} = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ MerchantServiceUsageConfigGetDistances =. val getDistances,
        MerchantServiceUsageConfigGetRoutes =. val getRoutes,
        MerchantServiceUsageConfigSnapToRoad =. val snapToRoad,
        MerchantServiceUsageConfigGetPlaceName =. val getPlaceName,
        MerchantServiceUsageConfigGetPlaceDetails =. val getPlaceDetails,
        MerchantServiceUsageConfigAutoComplete =. val autoComplete,
        MerchantServiceUsageConfigUpdatedAt =. val now
      ]
    where_ $
      tbl ^. MerchantServiceUsageConfigTId ==. val (toKey merchantId)
