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
import qualified Domain.Types.Estimate as DEB
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Storage.Beam.EstimateBreakup as BeamEB
import Storage.Tabular.EstimateBreakup as SEB

findAllByEstimateId :: (Transactionable m) => Id Estimate -> DTypeBuilder m [EstimateBreakupT]
findAllByEstimateId estimateId =
  Esq.findAll' $ do
    estimateBreakup <- from $ table @SEB.EstimateBreakupT
    where_ $ estimateBreakup ^. EstimateBreakupEstimateId ==. val (toKey estimateId)
    return estimateBreakup

findAllByEstimateId' :: L.MonadFlow m => Id Estimate -> m [EstimateBreakup]
findAllByEstimateId' _ = error "Not implemented"

transformBeamEstimateBreakupToDomain :: BeamEB.EstimateBreakup -> EstimateBreakup
transformBeamEstimateBreakupToDomain BeamEB.EstimateBreakupT {..} = do
  let price =
        DEB.EstimateBreakupPrice
          { currency = priceCurrency,
            value = roundToIntegral priceValue
          }
  EstimateBreakup
    { id = Id id,
      estimateId = Id estimateId,
      title = title,
      price = price
    }

transformDomainEstimateBreakupToBeam :: EstimateBreakup -> BeamEB.EstimateBreakup
transformDomainEstimateBreakupToBeam EstimateBreakup {..} =
  BeamEB.defaultEstimateBreakup
    { BeamEB.id = getId id,
      BeamEB.estimateId = getId estimateId,
      BeamEB.title = title,
      BeamEB.priceCurrency = price.currency,
      BeamEB.priceValue = realToFrac price.value
    }
