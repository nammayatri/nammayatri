module Product.Ride where

import App.Types
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Types.API.Ride as API
import Types.Error
import qualified Types.Storage.Person as SPerson
import Utils.Common
import qualified Types.Storage.Ride as SRide
import qualified Storage.Queries.Ride as QRide

getDriverLoc :: Id SRide.Ride -> Id SPerson.Person -> FlowHandler API.GetDriverLocRes
getDriverLoc rideId personId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  baseUrl <- xProviderUri <$> ask
  ride <- QRide.findById rideId >>= fromMaybeM PIDoesNotExist
  res <- ExternalAPI.location baseUrl (getId ride.productInstanceId)
  return $ makeGetDriverLocRes res.currPoint
  where
    makeGetDriverLocRes LatLong {..} =
      API.GetDriverLocRes {long = lon, ..}