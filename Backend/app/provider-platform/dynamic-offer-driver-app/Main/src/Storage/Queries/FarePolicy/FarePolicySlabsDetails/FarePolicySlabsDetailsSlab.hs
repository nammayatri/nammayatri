{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab where

import qualified Domain.Types.FarePolicy as DFP
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab

findAll' ::
  ( Transactionable m,
    Monad m,
    MonadThrow m,
    Log m
  ) =>
  Id DFP.FarePolicy ->
  DTypeBuilder m [FarePolicySlabsDetailsSlabT]
findAll' farePolicyId = do
  Esq.findAll' $ do
    farePolicySlabsDetailsSlab <- from $ table @FarePolicySlabsDetailsSlabT
    where_ $
      farePolicySlabsDetailsSlab ^. FarePolicySlabsDetailsSlabFarePolicyId ==. val (toKey farePolicyId)
    orderBy [asc $ farePolicySlabsDetailsSlab ^. FarePolicySlabsDetailsSlabStartDistance]
    return farePolicySlabsDetailsSlab

deleteAll' :: Id DFP.FarePolicy -> FullEntitySqlDB ()
deleteAll' farePolicyId =
  Esq.delete' $ do
    farePolicySlabsDetailsSlab <- from $ table @FarePolicySlabsDetailsSlabT
    where_ $
      farePolicySlabsDetailsSlab ^. FarePolicySlabsDetailsSlabFarePolicyId ==. val (toKey farePolicyId)
