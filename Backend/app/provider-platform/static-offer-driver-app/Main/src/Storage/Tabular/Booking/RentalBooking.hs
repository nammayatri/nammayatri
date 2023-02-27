{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Booking.RentalBooking where

import qualified Domain.Types.Booking.Type as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Booking.Table
import Storage.Tabular.FarePolicy.RentalFarePolicy (RentalFarePolicyTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RentalBookingT sql=rental_booking
      bookingId BookingTId
      rentalFarePolicyId RentalFarePolicyTId
      Primary bookingId
      deriving Generic
    |]

instance TEntityKey RentalBookingT where
  type DomainKey RentalBookingT = Id Domain.Booking
  fromKey (RentalBookingTKey _id) = fromKey _id
  toKey id = RentalBookingTKey $ toKey id
