{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Merchant
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant as DM
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.Merchant

findById :: forall m ma. Transactionable ma m => Id Merchant -> Proxy ma -> m (Maybe Merchant)
findById merchantId _ = Esq.findById @m @ma merchantId

findBySubscriberId :: forall m ma. Transactionable ma m => ShortId Subscriber -> Proxy ma -> m (Maybe Merchant)
findBySubscriberId subscriberId _ = Esq.findOne @m @ma $ do
  org <- from $ table @MerchantT
  where_ $
    org ^. MerchantSubscriberId ==. val (subscriberId.getShortId)
  return org

findByShortId :: forall m ma. Transactionable ma m => ShortId Merchant -> Proxy ma -> m (Maybe Merchant)
findByShortId shortId _ = Esq.findOne @m @ma $ do
  org <- from $ table @MerchantT
  where_ $
    org ^. MerchantShortId ==. val (shortId.getShortId)
  return org

loadAllProviders :: forall m ma. Transactionable ma m => Proxy ma -> m [Merchant]
loadAllProviders _ =
  Esq.findAll @m @ma $ do
    org <- from $ table @MerchantT
    where_ $
      org ^. MerchantStatus ==. val DM.APPROVED
        &&. org ^. MerchantEnabled
    return org

update :: Merchant -> SqlDB m ()
update org = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ MerchantName =. val org.name,
        MerchantDescription =. val org.description,
        MerchantHeadCount =. val org.headCount,
        MerchantEnabled =. val org.enabled,
        MerchantUpdatedAt =. val now,
        MerchantFromTime =. val org.fromTime
      ]
    where_ $ tbl ^. MerchantTId ==. val (toKey org.id)
