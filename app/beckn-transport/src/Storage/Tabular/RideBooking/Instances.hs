{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RideBooking.Instances where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Booking.Type as Domain
import Storage.Tabular.FareProduct ()
import Storage.Tabular.RideBooking.RentalRideBooking
import Storage.Tabular.RideBooking.Table
import Storage.Tabular.Vehicle ()
import Types.Error
import Utils.Common hiding (id)

data BookingDetailsT = OneWayDetailsT | RentalDetailsT RentalRideBookingT

type FullBookingT = (RideBookingT, BookingDetailsT)

instance TType FullBookingT Domain.RideBooking where
  fromTType (RideBookingT {..}, bookingDetails) = do
    pUrl <- parseBaseUrl bapUri
    rideBookingDetails <- case bookingDetails of
      OneWayDetailsT -> do
        toLocationId' <- fromKey <$> toLocationId & fromMaybeM (RideBookingFieldNotPresent "estimatedDistance")
        estimatedDistance' <-
          estimatedDistance & fromMaybeM (RideBookingFieldNotPresent "estimatedDistance")
        pure $
          Domain.OneWayDetails
            Domain.OneWayRideBookingDetails
              { estimatedDistance = HighPrecMeters estimatedDistance',
                toLocationId = toLocationId'
              }
      RentalDetailsT rentalRideBookingT -> do
        return . Domain.RentalDetails $ fromRentalRideBookingTType rentalRideBookingT

    return $
      Domain.RideBooking
        { id = Id id,
          riderId = fromKey <$> riderId,
          fromLocationId = fromKey fromLocationId,
          providerId = fromKey providerId,
          bapUri = pUrl,
          ..
        }
  toTType Domain.RideBooking {..} = do
    let (detailsT, toLocationId, estimatedDistance) = case rideBookingDetails of
          Domain.OneWayDetails details -> (OneWayDetailsT, Just details.toLocationId, Just details.estimatedDistance)
          Domain.RentalDetails details -> (RentalDetailsT $ toRentalRideBookingTType id details, Nothing, Nothing)
    let rideBookingT =
          RideBookingT
            { id = getId id,
              riderId = toKey <$> riderId,
              fromLocationId = toKey fromLocationId,
              toLocationId = toKey <$> toLocationId,
              estimatedDistance = getHighPrecMeters <$> estimatedDistance,
              providerId = toKey providerId,
              bapUri = showBaseUrl bapUri,
              ..
            }
    (rideBookingT, detailsT)

fromRentalRideBookingTType :: RentalRideBookingT -> Domain.RentalRideBookingDetails
fromRentalRideBookingTType RentalRideBookingT {..} = do
  Domain.RentalRideBookingDetails
    { rentalFarePolicyId = fromKey rentalFarePolicyId
    }

toRentalRideBookingTType :: Id Domain.RideBooking -> Domain.RentalRideBookingDetails -> RentalRideBookingT
toRentalRideBookingTType rideBookingId Domain.RentalRideBookingDetails {..} =
  RentalRideBookingT
    { rideBookingId = toKey rideBookingId,
      rentalFarePolicyId = toKey rentalFarePolicyId
    }
