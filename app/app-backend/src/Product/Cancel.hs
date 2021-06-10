module Product.Cancel (cancel, onCancel) where

import App.Types
import Beckn.Types.APISuccess (APISuccess (Success))
import qualified Beckn.Types.Core.API.Cancel as API
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Models.Case as MC
import qualified Models.ProductInstance as MPI
import qualified Storage.Queries.Organization as OQ
import Types.API.Cancel as Cancel
import Types.Error
import qualified Types.Metrics as Metrics
import Utils.Common
import qualified Utils.Notifications as Notify

cancel :: Person.Person -> Cancel.CancelReq -> FlowHandler CancelRes
cancel person req = withFlowHandlerAPI $ do
  let entityType = req.entityType
  case entityType of
    Cancel.CASE -> searchCancel person req
    Cancel.PRODUCT_INSTANCE -> cancelProductInstance person req

cancelProductInstance :: Person.Person -> CancelReq -> Flow CancelRes
cancelProductInstance person req = do
  let prodInstId = req.entityId
  searchPI <- MPI.findById (Id prodInstId) -- TODO: Handle usecase where multiple productinstances exists for one product
  cs <- MC.findIdByPerson person (searchPI.caseId)
  orderPI <- MPI.findByParentIdType (searchPI.id) Case.RIDEORDER
  unless (isProductInstanceCancellable orderPI) $
    throwError $ PIInvalidStatus "Cannot cancel this ride"
  let txnId = getId $ cs.id
  let cancelReqMessage = API.CancelReqMessage (API.CancellationOrder prodInstId Nothing)
  context <- buildContext "cancel" txnId Nothing Nothing
  organization <-
    OQ.findOrganizationById (searchPI.organizationId)
      >>= fromMaybeM OrgNotFound
  baseUrl <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  ExternalAPI.cancel baseUrl (API.CancelReq context cancelReqMessage)
  return Success

searchCancel :: Person.Person -> CancelReq -> Flow CancelRes
searchCancel person req = do
  let caseId = req.entityId
  case_ <- MC.findIdByPerson person (Id caseId)
  unless (isCaseCancellable case_) $ throwError (CaseInvalidStatus "Cannot cancel with this case")
  Metrics.incrementCaseCount Case.CLOSED Case.RIDESEARCH
  piList <- MPI.findAllByCaseId (case_.id)
  traverse_ (`MPI.updateStatus` PI.CANCELLED) (PI.id <$> filter isProductInstanceCancellable piList)
  Case.validateStatusTransition (case_.status) Case.CLOSED & fromEitherM CaseInvalidStatus
  MC.updateStatus (case_.id) Case.CLOSED
  return Success

isProductInstanceCancellable :: PI.ProductInstance -> Bool
isProductInstanceCancellable prodInst =
  isRight $ PI.validateStatusTransition (prodInst.status) PI.CANCELLED

isCaseCancellable :: Case.Case -> Bool
isCaseCancellable case_ =
  case case_.status of
    Case.NEW -> True
    Case.CONFIRMED -> True
    _ -> False

onCancel :: Organization.Organization -> API.OnCancelReq -> FlowHandler API.OnCancelRes
onCancel _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "on_cancel" $ req.context
    case req.contents of
      Right msg -> do
        let prodInstId = Id $ msg.id
        -- TODO: Handle usecase where multiple productinstances exists for one product

        piList <- MPI.findAllByParentId prodInstId
        case piList of
          [] -> return ()
          s : _ -> do
            let orderPi = s
            -- TODO what if we update several PI but then get an error?
            -- wrap everything in a transaction
            PI.validateStatusTransition (PI.status orderPi) PI.CANCELLED & fromEitherM PIInvalidStatus
            MPI.updateStatus (PI.id orderPi) PI.CANCELLED
            case_ <- MC.findById $ PI.caseId orderPi
            Case.validateStatusTransition (case_.status) Case.CLOSED & fromEitherM CaseInvalidStatus
            MC.updateStatus (PI.caseId orderPi) Case.CLOSED
            return ()
        productInstance <- MPI.findById prodInstId
        PI.validateStatusTransition (PI.status productInstance) PI.CANCELLED & fromEitherM PIInvalidStatus
        MPI.updateStatus prodInstId PI.CANCELLED
        let caseId = productInstance.caseId
        -- notify customer
        Notify.notifyOnStatusUpdate productInstance PI.CANCELLED
        --
        arrPICase <- MPI.findAllByCaseId caseId
        let arrTerminalPI =
              filter
                ( \prodInst -> do
                    let status = prodInst.status
                    status == PI.COMPLETED
                      || status == PI.OUTOFSTOCK
                      || status == PI.CANCELLED
                      || status == PI.INVALID
                )
                arrPICase
        when
          (length arrTerminalPI == length arrPICase)
          ( do
              Metrics.incrementCaseCount Case.CLOSED Case.RIDEORDER
              case_ <- MC.findById caseId
              Case.validateStatusTransition (case_.status) Case.CLOSED & fromEitherM CaseInvalidStatus
              MC.updateStatus caseId Case.CLOSED
          )
      Left err -> logTagError "on_cancel req" $ "on_cancel error: " <> show err
    return Ack
