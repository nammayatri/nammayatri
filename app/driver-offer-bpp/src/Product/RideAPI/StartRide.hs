module Product.RideAPI.StartRide where

import Beckn.LocationUpdates
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong)
import Beckn.Utils.SlidingWindowLimiter (checkSlidingWindowLimit)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import Environment (FlowHandler)
import EulerHS.Prelude hiding (id)
import Product.BecknProvider.BP
import qualified Product.RideAPI.Handlers.StartRide as Handler
import SharedLogic.LocationUpdates
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Types.API.Ride (StartRideReq (..))
import Utils.Common (withFlowHandlerAPI)

startRide :: Id SP.Person -> Id SRide.Ride -> StartRideReq -> FlowHandler APISuccess.APISuccess
startRide personId rideId req = withFlowHandlerAPI $ do
  Handler.startRideHandler handle personId (cast rideId) req
  where
    handle =
      Handler.ServiceHandle
        { findById = QPerson.findById,
          findBookingById = QRB.findById,
          findRideById = QRide.findById,
          startRideAndUpdateLocation = startRideTransaction,
          notifyBAPRideStarted = sendRideStartedUpdateToBAP,
          rateLimitStartRide = \personId' rideId' -> checkSlidingWindowLimit (getId personId' <> "_" <> getId rideId'),
          addFirstWaypoint = \driverId pt -> do
            clearLocationUpdatesOnRideEnd defaultRideInterpolationHandler driverId
            addPoints defaultRideInterpolationHandler driverId $ pt :| []
        }

startRideTransaction :: EsqDBFlow m r => Id SRide.Ride -> Id SRB.Booking -> Id SP.Person -> LatLong -> m ()
startRideTransaction rideId bookingId driverId firstPoint = Esq.runTransaction $ do
  QRide.updateStatus rideId SRide.INPROGRESS
  QRide.updateStartTime rideId
  QBE.logRideCommencedEvent (cast driverId) bookingId rideId
  now <- getCurrentTime
  DrLoc.upsertGpsCoord driverId firstPoint now
