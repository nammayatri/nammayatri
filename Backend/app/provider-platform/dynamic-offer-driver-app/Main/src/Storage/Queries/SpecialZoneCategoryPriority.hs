{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.SpecialZoneCategoryPriority where

import Domain.Types.Merchant
import Domain.Types.SpecialZoneCategoryPriority
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.SpecialZoneCategoryPriority

findByMerchantIdAndCategory ::
  Transactionable m =>
  Id Merchant ->
  Text ->
  m (Maybe SpecialZoneCategoryPriority)
findByMerchantIdAndCategory merchantId category =
  Esq.findOne $ do
    specialZoneCategoryPriority <- from $ table @SpecialZoneCategoryPriorityT
    where_ $ specialZoneCategoryPriority ^. SpecialZoneCategoryPriorityMerchantId ==. val (toKey merchantId)
    where_ $ specialZoneCategoryPriority ^. SpecialZoneCategoryPriorityCategory ==. val category
    return specialZoneCategoryPriority
