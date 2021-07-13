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
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ProductInstance as QProductInstance
import Types.API.Ride (StartRideReq (..))
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Person as SP
import qualified Types.Storage.ProductInstance as ProductInstance
import Utils.Common (withFlowHandlerAPI)

startRide :: Id SP.Person -> Id ProductInstance.ProductInstance -> StartRideReq -> FlowHandler APISuccess.APISuccess
startRide personId rideId req = withFlowHandlerAPI $ do
  Handler.startRideHandler handle personId (cast rideId) (req.rideOtp)
  where
    handle =
      Handler.ServiceHandle
        { findPersonById = QPerson.findPersonById,
          findPIById = QProductInstance.findById,
          startRide = startRideTransaction,
          notifyBAPRideStarted = \searchPi orderPi -> notifyUpdateToBAP searchPi orderPi ProductInstance.INPROGRESS,
          rateLimitStartRide = \personId' rideId' -> checkSlidingWindowLimit (getId personId' <> "_" <> getId rideId')
        }

startRideTransaction :: DBFlow m r => Id ProductInstance.ProductInstance -> Id Case.Case -> m ()
startRideTransaction piId searchCaseId = DB.runSqlDBTransaction $ do
  QProductInstance.updateStatus piId ProductInstance.INPROGRESS
  QCase.updateStatus searchCaseId Case.INPROGRESS
