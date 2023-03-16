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

module Storage.Tabular.Ride.Instances where

import qualified Domain.Types.Ride as Domain
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Ride.Table

instance FromTType RideT Domain.Ride where
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

instance ToTType RideT Domain.Ride where
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
