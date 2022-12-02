module Storage.Queries.Merchant.MerchantServiceConfig where

import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (findById)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Merchant as DOrg
import Domain.Types.Merchant.MerchantServiceConfig (MerchantServiceConfig, ServiceName)
import Storage.Tabular.Merchant.MerchantServiceConfig

findByMerchantIdAndService :: Transactionable m => Id Merchant -> ServiceName -> m (Maybe MerchantServiceConfig)
findByMerchantIdAndService orgId service =
  Esq.findOne $ do
    orgMapsCfg <- from $ table @MerchantServiceConfigT
    where_ $
      orgMapsCfg ^. MerchantServiceConfigTId ==. val (toKey orgId)
        &&. orgMapsCfg ^. MerchantServiceConfigServiceName ==. val service
    return orgMapsCfg
