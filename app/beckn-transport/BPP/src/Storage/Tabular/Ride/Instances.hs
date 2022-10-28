{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Ride.Instances (FullRideT) where

import Beckn.External.Maps.Types (LatLong (..))
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
    let mbTripStartLoc = LatLong <$> tripStartLat <*> tripStartLon
    let mbTripEndLoc = LatLong <$> tripEndLat <*> tripEndLon
    return $
      Domain.Ride
        { id = Id id,
          bookingId = fromKey bookingId,
          shortId = ShortId shortId,
          driverId = fromKey driverId,
          chargeableDistance = roundToIntegral <$> chargeableDistance,
          trackingUrl = tUrl,
          fare = roundToIntegral <$> fare,
          totalFare = roundToIntegral <$> totalFare,
          tripStartPos = mbTripStartLoc,
          tripEndPos = mbTripEndLoc,
          ..
        }
  toTType Domain.Ride {..} = do
    let rideT =
          RideT
            { id = getId id,
              bookingId = toKey bookingId,
              shortId = getShortId shortId,
              driverId = toKey driverId,
              chargeableDistance = fromIntegral <$> chargeableDistance,
              trackingUrl = showBaseUrl trackingUrl,
              fare = fromIntegral <$> fare,
              totalFare = fromIntegral <$> totalFare,
              tripStartLat = tripStartPos <&> (.lat),
              tripStartLon = tripStartPos <&> (.lon),
              tripEndLat = tripEndPos <&> (.lat),
              tripEndLon = tripEndPos <&> (.lon),
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
