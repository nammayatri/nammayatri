module Product.RideAPI.CancelRide where

import App.Types (FlowHandler)
import qualified Beckn.Types.APIResult as APIResult
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (getCurrTime, withFlowHandler)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Product.BecknProvider.BP as BecknProvider
import qualified Product.RideAPI.Handlers.CancelRide as Handler
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ProductInstance as QProductInstance

cancelRide :: SR.RegistrationToken -> Text -> FlowHandler APIResult.APIResult
cancelRide SR.RegistrationToken {..} rideId = withFlowHandler $ do
  Handler.cancelRideHandler handle _EntityId rideId
  where
    handle =
      Handler.ServiceHandle
        { findPIById = QProductInstance.findById,
          findPersonById = QPerson.findPersonById,
          cancelRide = BecknProvider.cancelRide,
          generateGUID = L.generateGUID,
          getCurrentTime = getCurrTime
        }
