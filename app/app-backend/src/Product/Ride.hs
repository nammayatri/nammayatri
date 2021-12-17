module Product.Ride where

import App.Types
import Beckn.Types.Id
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Ride as QRide
import qualified Types.API.Ride as API
import Types.Error
import qualified Types.Storage.Person as SPerson
import qualified Types.Storage.Ride as SRide
import Utils.Common

getDriverLoc :: Id SRide.Ride -> Id SPerson.Person -> FlowHandler API.GetDriverLocRes
getDriverLoc rideId personId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  baseUrl <- xProviderUri <$> ask
  ride <- QRide.findById rideId >>= fromMaybeM RideDoesNotExist
  res <- ExternalAPI.location baseUrl (getId ride.bppRideId)
  return res.currPoint
