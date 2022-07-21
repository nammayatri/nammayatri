{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Booking.Instances where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Booking.Type as Domain
import Storage.Tabular.Booking.RentalBooking
import Storage.Tabular.Booking.Table
import Storage.Tabular.FarePolicy.FareProduct ()
import Storage.Tabular.Vehicle ()
import Types.Error
import Utils.Common hiding (id)

data BookingDetailsT = OneWayDetailsT | RentalDetailsT RentalBookingT

type FullBookingT = (BookingT, BookingDetailsT)

instance TType FullBookingT Domain.Booking where
  fromTType (BookingT {..}, bookingDetails_) = do
    pUrl <- parseBaseUrl bapUri
    bookingDetails <- case bookingDetails_ of
      OneWayDetailsT -> do
        toLocationId' <- fromKey <$> toLocationId & fromMaybeM (BookingFieldNotPresent "estimatedDistance")
        estimatedDistance' <-
          estimatedDistance & fromMaybeM (BookingFieldNotPresent "estimatedDistance")
        pure $
          Domain.OneWayDetails
            Domain.OneWayBookingDetails
              { estimatedDistance = HighPrecMeters estimatedDistance',
                toLocationId = toLocationId'
              }
      RentalDetailsT rentalBookingT -> do
        return . Domain.RentalDetails $ fromRentalBookingTType rentalBookingT

    return $
      Domain.Booking
        { id = Id id,
          riderId = fromKey <$> riderId,
          fromLocationId = fromKey fromLocationId,
          providerId = fromKey providerId,
          bapUri = pUrl,
          ..
        }
  toTType Domain.Booking {..} = do
    let (detailsT, toLocationId, estimatedDistance) = case bookingDetails of
          Domain.OneWayDetails details -> (OneWayDetailsT, Just details.toLocationId, Just details.estimatedDistance)
          Domain.RentalDetails details -> (RentalDetailsT $ toRentalBookingTType id details, Nothing, Nothing)
    let bookingT =
          BookingT
            { id = getId id,
              riderId = toKey <$> riderId,
              fromLocationId = toKey fromLocationId,
              toLocationId = toKey <$> toLocationId,
              estimatedDistance = getHighPrecMeters <$> estimatedDistance,
              providerId = toKey providerId,
              bapUri = showBaseUrl bapUri,
              ..
            }
    (bookingT, detailsT)

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
