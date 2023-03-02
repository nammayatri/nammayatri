{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.EstimateBreakup where

import Domain.Types.Estimate
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.EstimateBreakup as SEB

findAllByEstimateId :: forall m ma. (Transactionable ma m) => Id Estimate -> Proxy ma -> DTypeBuilder m [EstimateBreakupT]
findAllByEstimateId estimateId _ =
  Esq.findAll' @m @ma $ do
    estimateBreakup <- from $ table @SEB.EstimateBreakupT
    where_ $ estimateBreakup ^. EstimateBreakupEstimateId ==. val (toKey estimateId)
    return estimateBreakup
