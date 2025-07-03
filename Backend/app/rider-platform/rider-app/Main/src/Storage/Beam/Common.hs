{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.Common where

import qualified Database.Beam as B
import Kernel.Prelude (Generic)
import Storage.Beam.Booking
import Storage.Beam.BookingCancellationReason
import Storage.Beam.Exophone
import Storage.Beam.Geometry as BeamG
import Storage.Beam.Person
import Storage.Beam.Rating (RatingT, ratingTable)
import Storage.Beam.Ride

atlasDB :: B.DatabaseSettings be AtlasDB
atlasDB =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { exophone = exophoneTable,
        geometry = geometryTable,
        booking = bookingTable,
        ride = rideTable,
        person = personTable,
        bookingCancellationReason = bookingCancellationReasonTable,
        rating = ratingTable
      }

data AtlasDB f = AtlasDB
  { exophone :: f (B.TableEntity ExophoneT),
    geometry :: f (B.TableEntity GeometryT),
    booking :: f (B.TableEntity BookingT),
    ride :: f (B.TableEntity RideT),
    person :: f (B.TableEntity PersonT),
    bookingCancellationReason :: f (B.TableEntity BookingCancellationReasonT),
    rating :: f (B.TableEntity RatingT)
  }
  deriving (Generic, B.Database be)
