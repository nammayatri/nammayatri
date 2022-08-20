{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Ride where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.Ride as Domain
import qualified Domain.Types.VehicleVariant as VehVar (VehicleVariant)
import qualified Storage.Tabular.Booking as SRB

derivePersistField "Domain.RideStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideT sql=ride
      id Text
      bppRideId Text
      bookingId SRB.BookingTId
      shortId Text
      status Domain.RideStatus
      driverName Text
      driverRating Double Maybe
      driverMobileNumber Text
      driverRegisteredAt UTCTime
      vehicleNumber Text
      vehicleModel Text
      vehicleColor Text
      vehicleVariant VehVar.VehicleVariant
      otp Text
      trackingUrl Text Maybe
      fare Amount Maybe
      totalFare Amount Maybe
      chargeableDistance Double Maybe
      rideStartTime UTCTime Maybe
      rideEndTime UTCTime Maybe
      rideRating Int Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      Unique RideShortId
      deriving Generic
    |]

instance TEntityKey RideT where
  type DomainKey RideT = Id Domain.Ride
  fromKey (RideTKey _id) = Id _id
  toKey (Id id) = RideTKey id

instance TType RideT Domain.Ride where
  fromTType RideT {..} = do
    tUrl <- parseBaseUrl `mapM` trackingUrl
    return $
      Domain.Ride
        { id = Id id,
          bppRideId = Id bppRideId,
          bookingId = fromKey bookingId,
          shortId = ShortId shortId,
          trackingUrl = tUrl,
          ..
        }
  toTType Domain.Ride {..} =
    RideT
      { id = getId id,
        bppRideId = getId bppRideId,
        bookingId = toKey bookingId,
        shortId = getShortId shortId,
        trackingUrl = showBaseUrl <$> trackingUrl,
        ..
      }
