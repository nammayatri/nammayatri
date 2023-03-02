{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.MerchantAccess where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantAccess as DAccess
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Merchant
import Storage.Tabular.MerchantAccess

create :: DAccess.MerchantAccess -> SqlDB m ()
create = Esq.create

findByPersonIdAndMerchantId ::
  forall m ma.
  Transactionable ma m =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  Proxy ma ->
  m (Maybe DAccess.MerchantAccess)
findByPersonIdAndMerchantId personId merchantId _ = findOne @m @ma $ do
  merchantAccess <- from $ table @MerchantAccessT
  where_ $
    merchantAccess ^. MerchantAccessPersonId ==. val (toKey personId)
      &&. merchantAccess ^. MerchantAccessMerchantId ==. val (toKey merchantId)
  return merchantAccess

findAllByPersonId ::
  forall m ma.
  Transactionable ma m =>
  Id DP.Person ->
  Proxy ma ->
  m [DMerchant.Merchant]
findAllByPersonId personId _ = findAll @m @ma $ do
  (merchantAccess :& merchant) <-
    from $
      table @MerchantAccessT
        `innerJoin` table @MerchantT
          `Esq.on` ( \(merchantAccess :& merchant) ->
                       merchantAccess ^. MerchantAccessMerchantId ==. merchant ^. MerchantTId
                   )
  where_ $
    merchantAccess ^. MerchantAccessPersonId ==. val (toKey personId)
  return merchant

deleteById :: Id DAccess.MerchantAccess -> SqlDB m ()
deleteById = Esq.deleteByKey @MerchantAccessT
