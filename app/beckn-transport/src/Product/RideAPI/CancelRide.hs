module Product.RideAPI.CancelRide where

import App.Types
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import EulerHS.Prelude hiding (id)
import qualified Product.BecknProvider.BP as BecknProvider
import qualified Product.RideAPI.Handlers.CancelRide as Handler
import qualified Storage.Queries.Person as QPerson
import Types.API.Ride (CancelRideReq)
import qualified Storage.Queries.Ride as QRide
import qualified Types.Storage.OldRide as Ride
import Utils.Common (withFlowHandlerAPI)
import qualified Types.Storage.Person as SP

cancelRide :: Id SP.Person -> Id Ride.Ride -> CancelRideReq -> FlowHandler APISuccess.APISuccess
cancelRide personId rideId req = withFlowHandlerAPI $ do
  Handler.cancelRideHandler handle personId rideId req
  where
    handle =
      Handler.ServiceHandle
        { findRideById = QRide.findById,
          findPersonById = QPerson.findPersonById,
          cancelRide = BecknProvider.cancelRide
        }
