{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Location
  ( Status (..),
    GetLocationRes (..),
    getLocation,
  )
where

import qualified Domain.Types.Ride as SRide
import GHC.Records.Extra
import Kernel.Beam.Functions
import qualified Kernel.External.Maps.HasCoordinates as GoogleMaps
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified SharedLogic.DriverLocation as DrLoc
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide

data Status = PreRide | ActualRide
  deriving (Generic, ToJSON, Show, FromJSON, ToSchema)

data GetLocationRes = GetLocationRes
  { currPoint :: LatLong,
    totalDistance :: Double,
    status :: Status,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

getLocation :: (EsqDBReplicaFlow m r, CacheFlow m r, EsqDBFlow m r, MonadReader r m, HasField "enableLocationTrackingService" r Bool) => Id SRide.Ride -> m GetLocationRes
getLocation rideId = do
  ride <-
    runInReplica $
      QRide.findById rideId
        >>= fromMaybeM (RideDoesNotExist rideId.getId)
  status <-
    case ride.status of
      SRide.NEW -> pure PreRide
      SRide.INPROGRESS -> pure ActualRide
      _ -> throwError $ RideInvalidStatus "Cannot track this ride"
  driver <- runInReplica $ Person.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  currLocation <- DrLoc.findById driver.id >>= fromMaybeM LocationNotFound
  let lastUpdate = currLocation.updatedAt
  let totalDistance = realToFrac ride.traveledDistance.getHighPrecMeters
      currPoint = GoogleMaps.getCoordinates currLocation
  return $ GetLocationRes {..}
