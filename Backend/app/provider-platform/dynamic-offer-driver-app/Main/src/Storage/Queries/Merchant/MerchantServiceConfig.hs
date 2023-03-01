{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Merchant.MerchantServiceConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant as DOrg
import Domain.Types.Merchant.MerchantServiceConfig (MerchantServiceConfig, ServiceName)
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (findById)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.Merchant.MerchantServiceConfig

findByMerchantIdAndService :: forall m ma. Transactionable ma m => Id Merchant -> ServiceName -> Proxy ma -> m (Maybe MerchantServiceConfig)
findByMerchantIdAndService merchantId serviceName _ =
  Esq.findOne @m @ma $ do
    merchantServiceConfig <- from $ table @MerchantServiceConfigT
    where_ $
      merchantServiceConfig ^. MerchantServiceConfigTId ==. val (toKey (merchantId, serviceName))
    return merchantServiceConfig

upsertMerchantServiceConfig :: MerchantServiceConfig -> SqlDB m ()
upsertMerchantServiceConfig merchantServiceConfig = do
  now <- getCurrentTime
  let (_serviceName, configJSON) = getServiceNameConfigJSON merchantServiceConfig.serviceConfig
  Esq.upsert
    merchantServiceConfig
    [ MerchantServiceConfigConfigJSON =. val configJSON,
      MerchantServiceConfigUpdatedAt =. val now
    ]
