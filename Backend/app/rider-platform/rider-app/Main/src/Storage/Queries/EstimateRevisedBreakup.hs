{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.EstimateRevisedBreakup where

import Domain.Types.EstimateRevised
import qualified Domain.Types.EstimateRevised as DEBR
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.EstimateRevisedBreakup as BeamEBR

create :: MonadFlow m => EstimateRevisedBreakup -> m ()
create = createWithKV

findAllByEstimateRevisedIdT :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id EstimateRevised -> m [EstimateRevisedBreakup]
findAllByEstimateRevisedIdT (Id estimateId) = findAllWithKV [Se.Is BeamEBR.estimateRevisedId $ Se.Eq estimateId]

instance FromTType' BeamEBR.EstimateRevisedBreakup EstimateRevisedBreakup where
  fromTType' BeamEBR.EstimateRevisedBreakupT {..} = do
    let price =
          DEBR.EstimateRevisedBreakupPrice
            { currency = priceCurrency,
              value = roundToIntegral priceValue
            }
    pure $
      Just
        EstimateRevisedBreakup
          { id = Id id,
            estimateRevisedId = Id estimateRevisedId,
            title = title,
            price = price
          }

instance ToTType' BeamEBR.EstimateRevisedBreakup EstimateRevisedBreakup where
  toTType' EstimateRevisedBreakup {..} = do
    BeamEBR.EstimateRevisedBreakupT
      { BeamEBR.id = getId id,
        BeamEBR.estimateRevisedId = getId estimateRevisedId,
        BeamEBR.title = title,
        BeamEBR.priceCurrency = price.currency,
        BeamEBR.priceValue = realToFrac price.value
      }
