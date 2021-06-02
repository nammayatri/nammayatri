{-# LANGUAGE OverloadedLabels #-}

module Product.RideAPI.StartRide where

import App.Types (AppEnv (..), Flow, FlowHandler)
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.RegistrationToken as SR
import EulerHS.Prelude hiding (id)
import qualified Models.Case
import Product.ProductInstance (notifyUpdateToBAP)
import qualified Product.RideAPI.Handlers.StartRide as Handler
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ProductInstance as QProductInstance
import Types.API.Ride (StartRideReq (..))
import Utils.Common (withFlowHandlerAPI)

startRide :: SR.RegistrationToken -> Id ProductInstance.ProductInstance -> StartRideReq -> FlowHandler APISuccess.APISuccess
startRide SR.RegistrationToken {..} rideId req = withFlowHandlerAPI $ do
  Handler.startRideHandler handle (Id entityId) (cast rideId) (req ^. #otp)
  where
    handle =
      Handler.ServiceHandle
        { findPersonById = QPerson.findPersonById,
          findPIById = QProductInstance.findById,
          findPIsByParentId = QProductInstance.findAllByParentId,
          findCaseByIdsAndType = Models.Case.findByIdType,
          startRide = startRideTransaction,
          notifyBAPRideStarted = \searchPi orderPi -> notifyUpdateToBAP searchPi orderPi ProductInstance.INPROGRESS
        }

startRideTransaction :: [Id ProductInstance.ProductInstance] -> Id Case.Case -> Id Case.Case -> Flow ()
startRideTransaction piIds trackerCaseId orderCaseId = DB.runSqlDBTransaction $ do
  QProductInstance.updateStatusByIds piIds ProductInstance.INPROGRESS
  QCase.updateStatus trackerCaseId Case.INPROGRESS
  QCase.updateStatus orderCaseId Case.INPROGRESS
