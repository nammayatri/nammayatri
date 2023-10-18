{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SpecialZoneQuote where

import Domain.Types.SpecialZoneQuote
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.SpecialZoneQuote as BeamSZQ

createSpecialZoneQuote :: MonadFlow m => SpecialZoneQuote -> m ()
createSpecialZoneQuote = createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SpecialZoneQuote -> m (Maybe SpecialZoneQuote)
findById specialZoneQuoteId = findOneWithKV [Se.Is BeamSZQ.id $ Se.Eq (getId specialZoneQuoteId)]

instance FromTType' BeamSZQ.SpecialZoneQuote SpecialZoneQuote where
  fromTType' BeamSZQ.SpecialZoneQuoteT {..} = do
    pure $
      Just
        SpecialZoneQuote
          { id = Id id,
            quoteId = quoteId
          }

instance ToTType' BeamSZQ.SpecialZoneQuote SpecialZoneQuote where
  toTType' SpecialZoneQuote {..} = do
    BeamSZQ.SpecialZoneQuoteT
      { BeamSZQ.id = getId id,
        BeamSZQ.quoteId = quoteId
      }
