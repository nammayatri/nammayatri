{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.EstimateBreakup where

import Domain.Types.Estimate
import qualified Domain.Types.Estimate as DEB
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.EstimateBreakup as BeamEB

create :: (L.MonadFlow m, Log m) => EstimateBreakup -> m ()
create estimate = do
  createWithKV estimate

-- findAllByEstimateIdT :: (Transactionable m) => EstimateTId -> MaybeT (DTypeBuilder m) [EstimateBreakupT]
-- findAllByEstimateIdT = lift . findAllByEstimateId'

-- findAllByEstimateId' :: (Transactionable m) => EstimateTId -> DTypeBuilder m [EstimateBreakupT]
-- findAllByEstimateId' estimateTId = Esq.findAll' $ do
--   estimateBreakup <- from $ table @SEB.EstimateBreakupT
--   return estimateBreakup

findAllByEstimateIdT :: (L.MonadFlow m, Log m) => Id Estimate -> m [EstimateBreakup]
findAllByEstimateIdT (Id estimateId) = do
  findAllWithKV [Se.Is BeamEB.estimateId $ Se.Eq estimateId]

instance FromTType' BeamEB.EstimateBreakup EstimateBreakup where
  fromTType' BeamEB.EstimateBreakupT {..} = do
    let price =
          DEB.EstimateBreakupPrice
            { currency = priceCurrency,
              value = roundToIntegral priceValue
            }
    pure $
      Just
        EstimateBreakup
          { id = Id id,
            estimateId = Id estimateId,
            title = title,
            price = price
          }

instance ToTType' BeamEB.EstimateBreakup EstimateBreakup where
  toTType' EstimateBreakup {..} = do
    BeamEB.EstimateBreakupT
      { BeamEB.id = getId id,
        BeamEB.estimateId = getId estimateId,
        BeamEB.title = title,
        BeamEB.priceCurrency = price.currency,
        BeamEB.priceValue = realToFrac price.value
      }
