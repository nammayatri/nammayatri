{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Ride.Instances (FullRideT) where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Person as Domain
import qualified Domain.Types.Ride as Domain
import Storage.Tabular.Rating
import Storage.Tabular.Ride.Table

type FullRideT = (RideT, Maybe RatingT)

instance TType FullRideT Domain.Ride where
  fromTType (RideT {..}, mbRatingT) = do
    tUrl <- parseBaseUrl trackingUrl
    let rideRating = mkRideRating <$> mbRatingT
    return $
      Domain.Ride
        { id = Id id,
          bookingId = fromKey bookingId,
          shortId = ShortId shortId,
          driverId = fromKey driverId,
          trackingUrl = tUrl,
          ..
        }
  toTType Domain.Ride {..} = do
    let rideT =
          RideT
            { id = getId id,
              bookingId = toKey bookingId,
              shortId = getShortId shortId,
              driverId = toKey driverId,
              trackingUrl = showBaseUrl trackingUrl,
              ..
            }
    let mbRatingT = mkRatingT driverId id <$> rideRating
    (rideT, mbRatingT)

mkRideRating :: RatingT -> Domain.RideRating
mkRideRating RatingT {..} =
  Domain.RideRating
    { id = Id id,
      ..
    }

mkRatingT :: Id Domain.Person -> Id Domain.Ride -> Domain.RideRating -> RatingT
mkRatingT driverId rideId Domain.RideRating {..} =
  RatingT
    { id = getId id,
      driverId = toKey driverId,
      rideId = toKey rideId,
      ..
    }
