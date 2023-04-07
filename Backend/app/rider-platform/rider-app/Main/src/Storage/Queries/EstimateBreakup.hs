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

import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.Estimate as TEstimate
import Storage.Tabular.EstimateBreakup as SEB

-- internal queries for building domain types

findAllByEstimateIdT :: (Transactionable m) => EstimateTId -> MaybeT (DTypeBuilder m) [EstimateBreakupT]
findAllByEstimateIdT = lift . findAllByEstimateId'

findAllByEstimateId' :: (Transactionable m) => EstimateTId -> DTypeBuilder m [EstimateBreakupT]
findAllByEstimateId' estimateTId = Esq.findAll' $ do
  estimateBreakup <- from $ table @SEB.EstimateBreakupT
  where_ $ estimateBreakup ^. EstimateBreakupEstimateId ==. val estimateTId
  return estimateBreakup
