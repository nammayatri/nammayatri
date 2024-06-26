{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.AmbulanceQuoteBreakup where

import Domain.Types.AmbulanceDetails
import Domain.Types.Quote
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.AmbulanceQuoteBreakup as BeamAQB

create :: (MonadFlow m, EsqDBFlow m r) => AmbulanceQuoteBreakup -> m ()
create = createWithKV

findAllByQuoteIdT :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Quote -> m [AmbulanceQuoteBreakup]
findAllByQuoteIdT (Id quoteId) = findAllWithKVAndConditionalDB [Se.Is BeamAQB.quoteId $ Se.Eq quoteId] Nothing

instance FromTType' BeamAQB.AmbulanceQuoteBreakup AmbulanceQuoteBreakup where
  fromTType' BeamAQB.AmbulanceQuoteBreakupT {..} = do
    let price =
          AmbulanceQuoteBreakupPrice
            { value = mkPrice (Just currency) value
            }
    pure $
      Just
        AmbulanceQuoteBreakup
          { id = Id id,
            quoteId = Id quoteId,
            title = title,
            price = price
          }

instance ToTType' BeamAQB.AmbulanceQuoteBreakup AmbulanceQuoteBreakup where
  toTType' AmbulanceQuoteBreakup {..} = do
    BeamAQB.AmbulanceQuoteBreakupT
      { BeamAQB.id = getId id,
        BeamAQB.quoteId = getId quoteId,
        BeamAQB.title = title,
        BeamAQB.currency = price.value.currency,
        BeamAQB.value = price.value.amount
      }
