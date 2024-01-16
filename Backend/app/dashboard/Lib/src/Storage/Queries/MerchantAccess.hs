{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.MerchantAccess where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantAccess as DAccess
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Storage.Tabular.Merchant
import Storage.Tabular.MerchantAccess

create :: DAccess.MerchantAccess -> SqlDB ()
create = Esq.create

findByPersonIdAndMerchantId ::
  (Transactionable m) =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  m [DAccess.MerchantAccess]
findByPersonIdAndMerchantId personId merchantId = findAll $ do
  merchantAccess <- from $ table @MerchantAccessT
  where_ $
    merchantAccess ^. MerchantAccessPersonId ==. val (toKey personId)
      &&. merchantAccess ^. MerchantAccessMerchantId ==. val (toKey merchantId)
  return merchantAccess

findByPersonIdAndMerchantIdAndCity ::
  (Transactionable m) =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  City.City ->
  m (Maybe DAccess.MerchantAccess)
findByPersonIdAndMerchantIdAndCity personId merchantId city = findOne $ do
  merchantAccess <- from $ table @MerchantAccessT
  where_ $
    merchantAccess ^. MerchantAccessPersonId ==. val (toKey personId)
      &&. merchantAccess ^. MerchantAccessMerchantId ==. val (toKey merchantId)
      &&. merchantAccess ^. MerchantAccessOperatingCity ==. val city
  return merchantAccess

findAllMerchantAccessByPersonId ::
  (Transactionable m) =>
  Id DP.Person ->
  m [DAccess.MerchantAccess]
findAllMerchantAccessByPersonId personId = findAll $ do
  merchantAccess <- from $ table @MerchantAccessT
  where_ $
    merchantAccess ^. MerchantAccessPersonId ==. val (toKey personId)
  return merchantAccess

findAllByPersonId ::
  (Transactionable m) =>
  Id DP.Person ->
  m [DMerchant.Merchant]
findAllByPersonId personId = findAll $ do
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

deleteById :: Id DAccess.MerchantAccess -> SqlDB ()
deleteById = Esq.deleteByKey @MerchantAccessT

updatePerson2faForMerchant :: Id DP.Person -> Id DMerchant.Merchant -> Text -> SqlDB ()
updatePerson2faForMerchant personId merchantId secretKey = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ MerchantAccessSecretKey =. val (Just secretKey),
        MerchantAccessIs2faEnabled =. val True
      ]
    where_ $
      tbl ^. MerchantAccessPersonId ==. val (toKey personId)
        &&. tbl ^. MerchantAccessMerchantId ==. val (toKey merchantId)

findAllUserAccountForMerchant ::
  (Transactionable m) =>
  Id DMerchant.Merchant ->
  m [DAccess.MerchantAccess]
findAllUserAccountForMerchant merchantId = findAll $ do
  merchantAccess <- from $ table @MerchantAccessT
  where_ $
    merchantAccess ^. MerchantAccessMerchantId ==. val (toKey merchantId)
  groupBy (merchantAccess ^. MerchantAccessPersonId)
  return merchantAccess
