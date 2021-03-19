module Product.RideAPI.StartRide where

import App.Types (AppEnv (dbCfg), Flow, FlowHandler)
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (withFlowHandler)
import EulerHS.Prelude
import qualified Models.Case
import Product.ProductInstance (notifyUpdateToBAP)
import qualified Product.RideAPI.Handlers.StartRide as Handler
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ProductInstance as QProductInstance

startRide :: SR.RegistrationToken -> Text -> Text -> FlowHandler APIResult.APIResult
startRide SR.RegistrationToken {..} rideId otp = withFlowHandler $ do
  Handler.startRideHandler handle _EntityId rideId otp
  where
    handle =
      Handler.ServiceHandle
        { findPersonById = QPerson.findPersonById,
          findPIById = QProductInstance.findById,
          findPIsByParentId = QProductInstance.findAllByParentId . Just,
          findCaseByIdsAndType = Models.Case.findByIdType,
          startRide = startRideTransaction,
          notifyBAPRideStarted = \searchPi orderPi -> notifyUpdateToBAP searchPi orderPi (Just ProductInstance.INPROGRESS)
        }

startRideTransaction :: [Id ProductInstance.ProductInstance] -> Id Case.Case -> Id Case.Case -> Flow ()
startRideTransaction piIds trackerCaseId orderCaseId = DB.runSqlDBTransaction $ do
  QProductInstance.updateStatusByIds piIds ProductInstance.INPROGRESS
  QCase.updateStatus trackerCaseId Case.INPROGRESS
  QCase.updateStatus orderCaseId Case.INPROGRESS
