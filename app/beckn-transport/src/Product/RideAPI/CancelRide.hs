module Product.RideAPI.CancelRide where

import App.Types
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude hiding (id)
import qualified Product.BecknProvider.Cancel as Cancel
import qualified Product.RideAPI.Handlers.CancelRide as Handler
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Types.API.Ride (CancelRideReq)
import Utils.Common (withFlowHandlerAPI)

cancelRide :: Id SP.Person -> Id SRide.Ride -> CancelRideReq -> FlowHandler APISuccess.APISuccess
cancelRide personId rideId req = withFlowHandlerAPI $ do
  Handler.cancelRideHandler handle personId rideId req
  where
    handle =
      Handler.ServiceHandle
        { findRideById = QRide.findById,
          findById = QPerson.findById,
          cancelRide = Cancel.cancelRide
        }
