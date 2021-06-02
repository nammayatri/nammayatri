module Product.RideAPI.EndRide where

import App.Types (AppEnv (..), Flow, FlowHandler)
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Beckn.Types.Storage.RegistrationToken as SR
import EulerHS.Prelude hiding (id)
import qualified Models.Case
import Product.ProductInstance as PI
import qualified Product.RideAPI.Handlers.EndRide as Handler
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as PI
import Types.App (Driver)
import Utils.Common (withFlowHandlerAPI)

endRide :: SR.RegistrationToken -> Id PI.ProductInstance -> FlowHandler APISuccess.APISuccess
endRide SR.RegistrationToken {..} rideId = withFlowHandlerAPI $ do
  Handler.endRideHandler handle (Id entityId) rideId
  where
    handle =
      Handler.ServiceHandle
        { findPersonById = Person.findPersonById,
          findPIById = PI.findById',
          findAllPIByParentId = PI.findAllByParentId,
          findCaseByIdAndType = Models.Case.findByIdType,
          notifyUpdateToBAP = PI.notifyUpdateToBAP,
          endRideTransaction
        }

endRideTransaction :: [Id PI.ProductInstance] -> Id Case.Case -> Id Case.Case -> Id Driver -> Flow ()
endRideTransaction piIds trackerCaseId orderCaseId driverId = DB.runSqlDBTransaction $ do
  PI.updateStatusByIds piIds PI.COMPLETED
  Case.updateStatus trackerCaseId Case.COMPLETED
  Case.updateStatus orderCaseId Case.COMPLETED
  DriverInformation.updateOnRide driverId False
  DriverStats.updateIdleTime driverId
