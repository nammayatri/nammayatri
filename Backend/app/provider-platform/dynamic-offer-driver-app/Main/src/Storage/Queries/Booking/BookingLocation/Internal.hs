{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Booking.BookingLocation.Internal where

import Domain.Types.Booking.BookingLocation as Domain
import EulerHS.Language (MonadFlow)
import Kernel.Beam.Functions (findAllWithKV)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging
import qualified Sequelize as Se
import Storage.Beam.Booking.BookingLocation as BeamBL
import Storage.Queries.Booking.BookingLocation ()

-- getBookingLocs ::
--   Transactionable m =>
--   [Id BookingLocation] ->
--   m [BookingLocation]
-- getBookingLocs locationIds = do
--   Esq.findAll $ do
--     bookingLoc <- from $ table @BookingLocationT
--     where_ $
--       bookingLoc ^. BookingLocationTId `in_` valList (toKey <$> locationIds)
--     return bookingLoc

getBookingLocs ::
  (MonadFlow m, Log m) =>
  [Id Domain.BookingLocation] ->
  m [Domain.BookingLocation]
getBookingLocs locationIds = findAllWithKV [Se.Is BeamBL.id $ Se.In $ getId <$> locationIds]
