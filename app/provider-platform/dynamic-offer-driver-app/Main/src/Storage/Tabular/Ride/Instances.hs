{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Ride.Instances where

import qualified Domain.Types.Ride as Domain
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Ride.Table

instance TType RideT Domain.Ride where
  fromTType RideT {..} = do
    tUrl <- parseBaseUrl trackingUrl
    let mbTripStartLoc = LatLong <$> tripStartLat <*> tripStartLon
    let mbTripEndLoc = LatLong <$> tripEndLat <*> tripEndLon
    return $
      Domain.Ride
        { id = Id id,
          bookingId = fromKey bookingId,
          shortId = ShortId shortId,
          driverId = fromKey driverId,
          trackingUrl = tUrl,
          tripStartPos = mbTripStartLoc,
          tripEndPos = mbTripEndLoc,
          fareParametersId = fromKey <$> fareParametersId,
          ..
        }
  toTType Domain.Ride {..} = do
    RideT
      { id = getId id,
        bookingId = toKey bookingId,
        shortId = getShortId shortId,
        driverId = toKey driverId,
        trackingUrl = showBaseUrl trackingUrl,
        tripStartLat = tripStartPos <&> (.lat),
        tripStartLon = tripStartPos <&> (.lon),
        tripEndLat = tripEndPos <&> (.lat),
        tripEndLon = tripEndPos <&> (.lon),
        fareParametersId = toKey <$> fareParametersId,
        ..
      }
