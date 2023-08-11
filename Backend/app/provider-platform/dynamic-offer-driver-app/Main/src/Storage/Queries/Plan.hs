{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Plan where

import Domain.Types.Merchant
import Domain.Types.Plan
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Plan

create :: Plan -> SqlDB ()
create = Esq.create

findByIdAndPaymentMode :: Transactionable m => Id Plan -> PaymentMode -> m (Maybe Plan)
findByIdAndPaymentMode planId paymentMode = Esq.findOne $ do
  plan <- from $ table @PlanT
  where_ $
    plan ^. PlanTId ==. val (toKey planId)
      &&. plan ^. PlanPaymentMode ==. val paymentMode
  return plan

findByMerchantId :: Transactionable m => Id Merchant -> m (Maybe Plan)
findByMerchantId merchantId = do
  findOne $ do
    plan <- from $ table @PlanT
    where_ $
      plan ^. PlanMerchantId ==. val (toKey merchantId)
    return plan

findByMerchantIdAndPaymentMode :: Transactionable m => Id Merchant -> PaymentMode -> m [Plan]
findByMerchantIdAndPaymentMode merchantId paymentMode = Esq.findAll $ do
  plan <- from $ table @PlanT
  where_ $
    plan ^. PlanMerchantId ==. val (toKey merchantId)
      &&. plan ^. PlanPaymentMode ==. val paymentMode
  return plan
