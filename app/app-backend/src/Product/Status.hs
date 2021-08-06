module Product.Status (status, onStatus) where

import App.Types
import Beckn.Types.APISuccess (APISuccess (Success))
import Beckn.Types.Common
import qualified Beckn.Types.Core.API.Status as API
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Utils.Error
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.ProductInstance as QPI
import Types.API.Status
import Types.Error
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as PI
import Utils.Common
import qualified Utils.Notifications as Notify

status :: Id Person.Person -> StatusReq -> FlowHandler StatusRes
status personId StatusReq {..} = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  prodInst <- QPI.findById (Id productInstanceId) >>= fromMaybeM PINotFound
  case_ <- Case.findIdByPersonId personId (prodInst.caseId) >>= fromMaybeM CaseNotFound
  let caseId = getId $ case_.id
  context <- buildContext "status" caseId Nothing Nothing
  organization <-
    OQ.findOrganizationById (prodInst.organizationId)
      >>= fromMaybeM OrgNotFound
  baseUrl <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let statusMessage = API.StatusReqMessage (IdObject productInstanceId) (IdObject caseId)
  ExternalAPI.status baseUrl (API.StatusReq context statusMessage)
  return Success

onStatus ::
  SignatureAuthResult Organization.Organization ->
  API.OnStatusReq ->
  FlowHandler API.OnStatusRes
onStatus _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    case req.contents of
      Right msg -> do
        let prodInstId = Id $ msg.order.id
            orderState = fromBeckn $ msg.order.state
        updateProductInstanceStatus prodInstId orderState
      Left err -> logTagError "on_status req" $ "on_status error: " <> show err
    return Ack
  where
    updateProductInstanceStatus prodInstId piStatus = do
      orderPi <- QPI.findByParentIdType prodInstId Case.RIDEORDER >>= fromMaybeM PIDoesNotExist
      PI.validateStatusTransition (PI.status orderPi) piStatus & fromEitherM PIInvalidStatus
      QPI.updateStatus (orderPi.id) piStatus
      Notify.notifyOnStatusUpdate orderPi piStatus
