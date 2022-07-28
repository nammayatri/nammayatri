{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Booking.Instances where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking.Type as Domain
import Storage.Tabular.Booking.BookingLocation
import Storage.Tabular.Booking.RentalBooking
import Storage.Tabular.Booking.Table
import Storage.Tabular.FarePolicy.FareProduct ()
import Storage.Tabular.Vehicle ()
import Tools.Error

data BookingDetailsT = OneWayDetailsT BookingLocationT | RentalDetailsT RentalBookingT

type FullBookingT = (BookingT, BookingLocationT, BookingDetailsT)

instance TType FullBookingT Domain.Booking where
  fromTType (BookingT {..}, fromLocT, bookingDetailsT) = do
    pUrl <- parseBaseUrl bapUri
    fromLocation <- fromTType fromLocT
    bookingDetails <- case bookingDetailsT of
      OneWayDetailsT toLocT -> do
        toLocation <- fromTType toLocT
        estimatedDistance' <-
          estimatedDistance & fromMaybeM (BookingFieldNotPresent "estimatedDistance")
        pure $
          Domain.OneWayDetails
            Domain.OneWayBookingDetails
              { estimatedDistance = HighPrecMeters estimatedDistance',
                toLocation
              }
      RentalDetailsT rentalBookingT -> do
        return . Domain.RentalDetails $ fromRentalBookingTType rentalBookingT

    return $
      Domain.Booking
        { id = Id id,
          riderId = fromKey <$> riderId,
          providerId = fromKey providerId,
          bapUri = pUrl,
          ..
        }
  toTType Domain.Booking {..} = do
    let (detailsT, toLocationId, estimatedDistance) = case bookingDetails of
          Domain.OneWayDetails details -> do
            let toLocT = toTType details.toLocation
            (OneWayDetailsT toLocT, Just . toKey $ details.toLocation.id, Just details.estimatedDistance)
          Domain.RentalDetails details -> do
            let rentalBookingT = toRentalBookingTType id details
            (RentalDetailsT rentalBookingT, Nothing, Nothing)
    let bookingT =
          BookingT
            { id = getId id,
              riderId = toKey <$> riderId,
              fromLocationId = toKey fromLocation.id,
              estimatedDistance = getHighPrecMeters <$> estimatedDistance,
              providerId = toKey providerId,
              bapUri = showBaseUrl bapUri,
              ..
            }
    let fromLocT = toTType fromLocation
    (bookingT, fromLocT, detailsT)

fromRentalBookingTType :: RentalBookingT -> Domain.RentalBookingDetails
fromRentalBookingTType RentalBookingT {..} = do
  Domain.RentalBookingDetails
    { rentalFarePolicyId = fromKey rentalFarePolicyId
    }

toRentalBookingTType :: Id Domain.Booking -> Domain.RentalBookingDetails -> RentalBookingT
toRentalBookingTType bookingId Domain.RentalBookingDetails {..} =
  RentalBookingT
    { bookingId = toKey bookingId,
      rentalFarePolicyId = toKey rentalFarePolicyId
    }
