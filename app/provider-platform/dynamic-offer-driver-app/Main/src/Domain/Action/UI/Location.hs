module Domain.Action.UI.Location
  ( Status (..),
    GetLocationRes (..),
    getLocation,
  )
where

import qualified Domain.Types.Ride as SRide
import GHC.Records.Extra
import qualified Kernel.External.Maps.HasCoordinates as GoogleMaps
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified SharedLogic.DriverLocation as DrLoc
import Storage.CachedQueries.CacheConfig (CacheFlow)
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

getLocation :: (EsqDBReplicaFlow m r, CacheFlow m r, EsqDBFlow m r) => Id SRide.Ride -> m GetLocationRes
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
