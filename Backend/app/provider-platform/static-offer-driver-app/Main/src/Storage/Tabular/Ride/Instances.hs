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

module Storage.Tabular.Ride.Instances (FullRideT) where

import qualified Domain.Types.Person as Domain
import qualified Domain.Types.Ride as Domain
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Rating
import Storage.Tabular.Ride.Table

type FullRideT = (RideT, Maybe RatingT)

instance FromTType FullRideT Domain.Ride where
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

instance ToTType FullRideT Domain.Ride where
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
