module Storage.Queries.Merchant.MerchantServiceUsageConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant as DOrg
import Domain.Types.Merchant.MerchantServiceUsageConfig
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (findById)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
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
        MerchantServiceUsageConfigSmsProvidersPriorityList =. val (PostgresList smsProvidersPriorityList),
        MerchantServiceUsageConfigUpdatedAt =. val now
      ]
    where_ $
      tbl ^. MerchantServiceUsageConfigTId ==. val (toKey merchantId)
