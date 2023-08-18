{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FareBreakup where

import Domain.Types.Booking.Type
import Domain.Types.FarePolicy.FareBreakup
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy.FareBreakup as BeamFB

create :: (L.MonadFlow m, Log m) => FareBreakup -> m ()
create = createWithKV

createMany :: (L.MonadFlow m, Log m) => [FareBreakup] -> m ()
createMany = traverse_ create

findAllByBookingId :: (L.MonadFlow m, Log m) => Id Booking -> m [FareBreakup]
findAllByBookingId bookingId = findAllWithKV [Se.Is BeamFB.bookingId $ Se.Eq $ getId bookingId]

instance FromTType' BeamFB.FareBreakup FareBreakup where
  fromTType' BeamFB.FareBreakupT {..} = do
    pure $
      Just
        FareBreakup
          { id = Id id,
            bookingId = Id bookingId,
            description = description,
            amount = amount
          }

instance ToTType' BeamFB.FareBreakup FareBreakup where
  toTType' FareBreakup {..} = do
    BeamFB.FareBreakupT
      { BeamFB.id = getId id,
        BeamFB.bookingId = getId bookingId,
        BeamFB.description = description,
        BeamFB.amount = amount
      }
