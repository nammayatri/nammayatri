module Domain.Action.UI.Location (Status (..), GetLocationRes (..), getLocation) where

import qualified Beckn.External.Maps.HasCoordinates as GoogleMaps
import Beckn.External.Maps.Types
import Beckn.Prelude
import Beckn.Storage.Esqueleto (runInReplica)
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common hiding (id)
import qualified Domain.Types.Ride as SRide
import GHC.Records.Extra
import qualified SharedLogic.DriverLocation as DrLoc
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide

data Status = PreRide | ActualRide
  deriving (Generic, ToJSON, Show, FromJSON, ToSchema)

data GetLocationRes = GetLocationRes
  { currPoint :: LatLong,
    totalDistance :: Meters,
    status :: Status,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

getLocation :: (EsqDBReplicaFlow m r, CacheFlow m r) => Id SRide.Ride -> m GetLocationRes
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
  let totalDistance = roundToIntegral ride.traveledDistance.getHighPrecMeters
      currPoint = GoogleMaps.getCoordinates currLocation
  return $ GetLocationRes {..}
