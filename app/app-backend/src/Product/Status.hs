{-# LANGUAGE OverloadedLabels #-}

module Product.Status (status, onStatus) where

import App.Types
import Beckn.Types.APISuccess (APISuccess (Success))
import Beckn.Types.Common
import qualified Beckn.Types.Core.API.Status as API
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Utils.Error
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Models.Case as Case
import qualified Models.ProductInstance as QPI
import qualified Storage.Queries.Organization as OQ
import Types.API.Status
import Types.Error
import Utils.Common
import qualified Utils.Notifications as Notify

status :: Person.Person -> StatusReq -> FlowHandler StatusRes
status person StatusReq {..} = withFlowHandlerAPI $ do
  prodInst <- QPI.findById (Id productInstanceId)
  case_ <- Case.findIdByPerson person (prodInst ^. #caseId)
  let caseId = getId $ case_ ^. #id
  context <- buildContext "status" caseId Nothing Nothing
  organization <-
    OQ.findOrganizationById (prodInst ^. #organizationId)
      >>= fromMaybeM OrgNotFound
  baseUrl <- organization ^. #callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let statusMessage = API.StatusReqMessage (IdObject productInstanceId) (IdObject caseId)
  ExternalAPI.status baseUrl (API.StatusReq context statusMessage)
  return Success

onStatus :: Organization.Organization -> API.OnStatusReq -> FlowHandler API.OnStatusRes
onStatus _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    case req ^. #contents of
      Right msg -> do
        let prodInstId = Id $ msg ^. #order . #id
            orderState = fromBeckn $ msg ^. #order . #state
        updateProductInstanceStatus prodInstId orderState
      Left err -> logTagError "on_status req" $ "on_status error: " <> show err
    return Ack
  where
    updateProductInstanceStatus prodInstId piStatus = do
      orderPi <- QPI.findByParentIdType prodInstId Case.RIDEORDER
      PI.validateStatusTransition (PI.status orderPi) piStatus & fromEitherM PIInvalidStatus
      QPI.updateStatus (orderPi ^. #id) piStatus
      Notify.notifyOnStatusUpdate orderPi piStatus
