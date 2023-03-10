{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Booking.Instances where

import qualified Domain.Types.Booking.Type as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Booking.BookingLocation
import Storage.Tabular.Booking.OneWayBooking
import Storage.Tabular.Booking.RentalBooking
import Storage.Tabular.Booking.Table
import Storage.Tabular.FarePolicy.FareProduct ()
import Storage.Tabular.Vehicle ()

data BookingDetailsT = OneWayDetailsT (BookingLocationT, OneWayBookingT) | RentalDetailsT RentalBookingT

type FullBookingT = (BookingT, BookingLocationT, BookingDetailsT)

instance FromTType FullBookingT Domain.Booking where
  fromTType (BookingT {..}, fromLocT, bookingDetailsT) = do
    pUrl <- parseBaseUrl bapUri
    let fromLocation = mkDomainBookingLocation fromLocT
    bookingDetails <- case bookingDetailsT of
      OneWayDetailsT detailsT -> do
        return . Domain.OneWayDetails $ fromOneWayDetailsT detailsT
      RentalDetailsT rentalBookingT -> do
        return . Domain.RentalDetails $ fromRentalDetailsT rentalBookingT

    return $
      Domain.Booking
        { id = Id id,
          riderId = fromKey <$> riderId,
          providerId = fromKey providerId,
          bapUri = pUrl,
          estimatedFare = roundToIntegral estimatedFare,
          estimatedTotalFare = roundToIntegral estimatedTotalFare,
          discount = roundToIntegral <$> discount,
          ..
        }

instance ToTType FullBookingT Domain.Booking where
  toTType Domain.Booking {..} = do
    let detailsT = case bookingDetails of
          Domain.OneWayDetails details -> OneWayDetailsT $ toOneWayDetailsT id details
          Domain.RentalDetails details -> RentalDetailsT $ toRentalDetailsT id details
    let bookingT =
          BookingT
            { id = getId id,
              fareProductType = Domain.getFareProductType bookingDetails,
              riderId = toKey <$> riderId,
              fromLocationId = toKey fromLocation.id,
              providerId = toKey providerId,
              bapUri = showBaseUrl bapUri,
              estimatedFare = fromIntegral estimatedFare,
              estimatedTotalFare = fromIntegral estimatedTotalFare,
              discount = fromIntegral <$> discount,
              ..
            }
    let fromLocT = mkTabularBookingLocation fromLocation
    (bookingT, fromLocT, detailsT)

fromOneWayDetailsT :: (BookingLocationT, OneWayBookingT) -> Domain.OneWayBookingDetails
fromOneWayDetailsT (toLocT, OneWayBookingT {..}) =
  Domain.OneWayBookingDetails
    { estimatedDistance = roundToIntegral estimatedDistance,
      estimatedFinishTime = estimatedFinishTime,
      toLocation = mkDomainBookingLocation toLocT,
      ..
    }

toOneWayDetailsT :: Id Domain.Booking -> Domain.OneWayBookingDetails -> (BookingLocationT, OneWayBookingT)
toOneWayDetailsT bookingId Domain.OneWayBookingDetails {..} =
  ( mkTabularBookingLocation toLocation,
    OneWayBookingT
      { bookingId = toKey bookingId,
        toLocationId = toKey toLocation.id,
        estimatedDistance = realToFrac estimatedDistance,
        ..
      }
  )

fromRentalDetailsT :: RentalBookingT -> Domain.RentalBookingDetails
fromRentalDetailsT RentalBookingT {..} = do
  Domain.RentalBookingDetails
    { rentalFarePolicyId = fromKey rentalFarePolicyId
    }

toRentalDetailsT :: Id Domain.Booking -> Domain.RentalBookingDetails -> RentalBookingT
toRentalDetailsT bookingId Domain.RentalBookingDetails {..} =
  RentalBookingT
    { bookingId = toKey bookingId,
      rentalFarePolicyId = toKey rentalFarePolicyId
    }
