module Storage.Queries.Merchant.MerchantServiceUsageConfig where

import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (findById)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Merchant as DOrg
import Domain.Types.Merchant.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
import Storage.Tabular.Merchant.MerchantServiceUsageConfig

findByMerchantId :: Transactionable m => Id Merchant -> m (Maybe MerchantServiceUsageConfig)
findByMerchantId orgId =
  Esq.findOne $ do
    orgMapsCfg <- from $ table @MerchantServiceUsageConfigT
    where_ $
      orgMapsCfg ^. MerchantServiceUsageConfigTId ==. val (toKey orgId)
    return orgMapsCfg
