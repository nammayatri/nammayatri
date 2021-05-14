module Product.RideAPI.CancelRide where

import App.Types (FlowHandler)
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import qualified Beckn.Types.Storage.ProductInstance as SPI
import qualified Beckn.Types.Storage.RegistrationToken as SR
import EulerHS.Prelude
import qualified Product.BecknProvider.BP as BecknProvider
import qualified Product.RideAPI.Handlers.CancelRide as Handler
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ProductInstance as QProductInstance
import Utils.Common (withFlowHandlerAPI)

cancelRide :: SR.RegistrationToken -> Id SPI.ProductInstance -> FlowHandler APISuccess.APISuccess
cancelRide SR.RegistrationToken {..} rideId = withFlowHandlerAPI $ do
  Handler.cancelRideHandler handle _EntityId $ cast rideId
  where
    handle =
      Handler.ServiceHandle
        { findPIById = QProductInstance.findById,
          findPersonById = QPerson.findPersonById,
          cancelRide = BecknProvider.cancelRide
        }
