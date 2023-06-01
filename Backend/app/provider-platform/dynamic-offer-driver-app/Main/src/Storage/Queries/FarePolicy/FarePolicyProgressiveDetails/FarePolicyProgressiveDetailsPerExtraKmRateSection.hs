{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection where

import qualified Domain.Types.FarePolicy as DFP
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection

findAll' ::
  ( Transactionable m,
    Monad m,
    MonadThrow m,
    Log m
  ) =>
  Id DFP.FarePolicy ->
  DTypeBuilder m [FarePolicyProgressiveDetailsPerExtraKmRateSectionT]
findAll' farePolicyId = do
  Esq.findAll' $ do
    farePolicyProgressiveDetailsPerExtraKmFareSection <- from $ table @FarePolicyProgressiveDetailsPerExtraKmRateSectionT
    where_ $
      farePolicyProgressiveDetailsPerExtraKmFareSection ^. FarePolicyProgressiveDetailsPerExtraKmRateSectionFarePolicyId ==. val (toKey farePolicyId)
    orderBy [asc $ farePolicyProgressiveDetailsPerExtraKmFareSection ^. FarePolicyProgressiveDetailsPerExtraKmRateSectionStartDistance]
    return farePolicyProgressiveDetailsPerExtraKmFareSection

deleteAll' :: Id DFP.FarePolicy -> FullEntitySqlDB ()
deleteAll' farePolicyId =
  Esq.delete' $ do
    farePolicyProgressiveDetailsPerExtraKmFareSection <- from $ table @FarePolicyProgressiveDetailsPerExtraKmRateSectionT
    where_ $
      farePolicyProgressiveDetailsPerExtraKmFareSection ^. FarePolicyProgressiveDetailsPerExtraKmRateSectionFarePolicyId ==. val (toKey farePolicyId)
