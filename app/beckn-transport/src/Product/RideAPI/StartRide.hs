module Product.RideAPI.StartRide where

import App.Types (FlowHandler)
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.SlidingWindowLimiter (checkSlidingWindowLimit)
import EulerHS.Prelude hiding (id)
import Product.BecknProvider.BP
import qualified Product.RideAPI.Handlers.StartRide as Handler
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import Types.API.Ride (StartRideReq (..))
import qualified Types.Storage.Person as SP
import qualified Types.Storage.OldRide as Ride
import Utils.Common (withFlowHandlerAPI)

startRide :: Id SP.Person -> Id Ride.Ride -> StartRideReq -> FlowHandler APISuccess.APISuccess
startRide personId rideId req = withFlowHandlerAPI $ do
  Handler.startRideHandler handle personId (cast rideId) (req.rideOtp)
  where
    handle =
      Handler.ServiceHandle
        { findPersonById = QPerson.findPersonById,
          findPIById = QQuote.findById,
          findRideById = QRide.findById,
          startRide = startRideTransaction,
          notifyBAPRideStarted = \quote rideId' -> notifyUpdateToBAP quote rideId' Ride.INPROGRESS,
          rateLimitStartRide = \personId' rideId' -> checkSlidingWindowLimit (getId personId' <> "_" <> getId rideId')
        }

startRideTransaction :: DBFlow m r => Id Ride.Ride -> m ()
startRideTransaction rideId = DB.runSqlDBTransaction $ do
  QRide.updateStatus rideId Ride.INPROGRESS
