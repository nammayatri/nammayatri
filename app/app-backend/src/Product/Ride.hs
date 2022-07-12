module Product.Ride where

import App.Types
import Beckn.Types.Id
import qualified Domain.Types.Person as SPerson
import Domain.Types.Ride
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as Euler
import Servant hiding (throwError)
import qualified Storage.Queries.Ride as QRide
import Types.API.Location
import qualified Types.API.Ride as API
import Types.Error
import Utils.Common

getDriverLoc :: Id SRide.Ride -> Id SPerson.Person -> FlowHandler API.GetDriverLocRes
getDriverLoc rideId personId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  when
    (ride.status == COMPLETED || ride.status == CANCELLED)
    $ throwError $ RideInvalidStatus "Cannot track this ride"
  trackingUrl <- ride.trackingUrl & fromMaybeM (RideFieldNotPresent "trackingUrl")
  let eulerClient = Euler.client (Proxy @(Get '[JSON] GetLocationRes))
  res <-
    callAPI trackingUrl eulerClient "BPP.driverTrackUrl"
      >>= fromEitherM (\err -> InternalError $ "Failed to call driverTrackUrl: " <> show err)
  return res.currPoint
