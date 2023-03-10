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

findById :: Transactionable m => Id Merchant -> m (Maybe Merchant)
findById = Esq.findById

findBySubscriberId :: Transactionable m => ShortId Subscriber -> m (Maybe Merchant)
findBySubscriberId subscriberId = Esq.findOne $ do
  org <- from $ table @MerchantT
  where_ $
    org ^. MerchantSubscriberId ==. val (subscriberId.getShortId)
  return org

findByShortId :: Transactionable m => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId = Esq.findOne $ do
  org <- from $ table @MerchantT
  where_ $
    org ^. MerchantShortId ==. val (shortId.getShortId)
  return org

loadAllProviders :: Transactionable m => m [Merchant]
loadAllProviders =
  Esq.findAll $ do
    org <- from $ table @MerchantT
    where_ $
      org ^. MerchantStatus ==. val DM.APPROVED
        &&. org ^. MerchantEnabled
    return org

update :: Merchant -> SqlDB ()
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

findByExoPhone :: Transactionable m => Text -> m (Maybe Merchant)
findByExoPhone exoPhone = do
  findOne $ do
    merchant <- from $ table @MerchantT
    where_ $
      Esq.val exoPhone
        `Esq.in_` subList_select
          ( do
              merchant1 <- from $ table @MerchantT
              where_ $ merchant1 ^. MerchantId ==. merchant ^. MerchantId
              return $ unnest (merchant1 ^. MerchantExoPhones)
          )
    return merchant
