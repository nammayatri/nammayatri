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

instance TType FullBookingT Domain.Booking where
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
