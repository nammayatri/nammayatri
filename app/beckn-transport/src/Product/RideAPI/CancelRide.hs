module Product.RideAPI.CancelRide where

import App.Types (FlowHandler)
import Beckn.Product.BusinessRule (runBRFlowFatal)
import qualified Beckn.Types.APIResult as APIResult
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (getCurrTime, withFlowHandler)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Product.RideAPI.Handlers.CancelRide as Handler
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ProductInstance as QProductInstance
import qualified Storage.Queries.RideRequest as QRideRequest

cancelRide :: SR.RegistrationToken -> Text -> FlowHandler APIResult.APIResult
cancelRide SR.RegistrationToken {..} rideId = withFlowHandler $ do
  runBRFlowFatal $ Handler.cancelRide handle _EntityId rideId
  where
    handle =
      Handler.ServiceHandle
        { findPIById = lift . QProductInstance.findById,
          findPersonById = lift . QPerson.findPersonById,
          createRideRequest = lift . QRideRequest.create,
          generateGUID = lift L.generateGUID,
          getCurrentTime = lift getCurrTime
        }
