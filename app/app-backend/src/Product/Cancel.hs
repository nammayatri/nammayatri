module Product.Cancel (cancel, onCancel) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.APISuccess (APISuccess (Success))
import qualified Beckn.Types.Core.API.Cancel as API
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Types.Mobility.Order (CancellationSource (..))
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Case as MC
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as MPI
import qualified Storage.Queries.RideCancellationReason as QRCR
import Types.API.Cancel as Cancel
import Types.Error
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as PI
import qualified Types.Storage.RideCancellationReason as SRCR
import Utils.Common
import qualified Utils.Metrics as Metrics
import qualified Utils.Notifications as Notify

cancel :: Id PI.ProductInstance -> Id Person.Person -> Cancel.CancelReq -> FlowHandler CancelRes
cancel bookingId personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  let orderPIId = bookingId
  rideCancellationReasonAPI <- req.rideCancellationReason & fromMaybeM (InvalidRequest "Cancellation reason is not present.")
  orderPI <- MPI.findOrderPIById orderPIId >>= fromMaybeM PIDoesNotExist
  searchPIId <- orderPI.parentId & fromMaybeM (PIFieldNotPresent "parentId")
  searchPI <- MPI.findById searchPIId >>= fromMaybeM PIDoesNotExist -- TODO: Handle usecase where multiple productinstances exists for one product
  searchCase <- MC.findByPersonId personId (searchPI.caseId) >>= fromMaybeM CaseNotFound
  unless (isProductInstanceCancellable orderPI) $
    throwError $ PIInvalidStatus "Cannot cancel this ride"
  let txnId = getId $ searchCase.id
  let cancelReqMessage = API.CancelReqMessage (API.CancellationOrder (getId searchPIId) Nothing)
  context <- buildContext "cancel" txnId Nothing Nothing
  organization <-
    OQ.findOrganizationById (searchPI.organizationId)
      >>= fromMaybeM OrgNotFound
  baseUrl <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  ExternalAPI.cancel baseUrl (API.CancelReq context cancelReqMessage)
  DB.runSqlDBTransaction $
    QRCR.create $ makeRideCancelationReason orderPI.id rideCancellationReasonAPI
  return Success
  where
    makeRideCancelationReason orderPIId rideCancellationReasonAPI = do
      let RideCancellationReasonAPIEntity {..} = rideCancellationReasonAPI
      SRCR.RideCancellationReason
        { rideId = cast orderPIId,
          source = ByUser,
          reasonCode = Just reasonCode,
          additionalInfo = additionalInfo
        }

isProductInstanceCancellable :: PI.ProductInstance -> Bool
isProductInstanceCancellable prodInst =
  isRight $ PI.validateStatusTransition (prodInst.status) PI.CANCELLED

onCancel ::
  SignatureAuthResult Organization.Organization ->
  API.OnCancelReq ->
  FlowHandler API.OnCancelRes
onCancel _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "on_cancel" $ req.context
    case req.contents of
      Right msg -> do
        let searchPIid = Id $ msg.order.id
        -- TODO: Handle usecase where multiple productinstances exists for one product

        mbOrderPI <- MPI.findByParentIdType searchPIid PI.RIDEORDER

        whenJust mbOrderPI $ \orderPI -> do
          -- TODO what if we update several PI but then get an error?
          -- wrap everything in a transaction
          PI.validateStatusTransition (PI.status orderPI) PI.CANCELLED & fromEitherM PIInvalidStatus
          MPI.updateStatus (PI.id orderPI) PI.CANCELLED
          orderCase <- MC.findById (PI.caseId orderPI) >>= fromMaybeM CaseDoesNotExist
          Case.validateStatusTransition (orderCase.status) Case.CLOSED & fromEitherM CaseInvalidStatus
          MC.updateStatusFlow (PI.caseId orderPI) Case.CLOSED
        searchPI <- MPI.findById searchPIid >>= fromMaybeM PIDoesNotExist
        PI.validateStatusTransition (PI.status searchPI) PI.CANCELLED & fromEitherM PIInvalidStatus
        MPI.updateStatus searchPIid PI.CANCELLED
        let searchCaseId = searchPI.caseId
        -- notify customer
        searchCase <- MC.findById searchCaseId >>= fromMaybeM CaseNotFound
        cancellationSource <- msg.order.cancellation_reason_id & fromMaybeM (InvalidRequest "No cancellation source.")
        logTagInfo ("txnId-" <> getId searchCaseId) ("Cancellation reason " <> show cancellationSource)
        whenJust (searchCase.requestor) $ \personId -> do
          mbPerson <- Person.findById $ Id personId
          whenJust mbPerson $ \person -> Notify.notifyOnCancel searchPI person.id person.deviceToken cancellationSource
          unless (cancellationSource == ByUser) $
            whenJust mbOrderPI $ \orderPI ->
              DB.runSqlDBTransaction $
                QRCR.create $ SRCR.RideCancellationReason orderPI.id cancellationSource Nothing Nothing
        --
        arrSearchPI <- MPI.findAllByCaseId searchCaseId
        let arrTerminalPI =
              filter
                ( \prodInst -> do
                    let status = prodInst.status
                    status == PI.COMPLETED
                      || status == PI.OUTOFSTOCK
                      || status == PI.CANCELLED
                      || status == PI.INVALID
                )
                arrSearchPI
        when
          (length arrTerminalPI == length arrSearchPI)
          ( do
              Metrics.incrementCaseCount Case.CLOSED Case.RIDESEARCH
              case_ <- MC.findById searchCaseId >>= fromMaybeM CaseDoesNotExist
              Case.validateStatusTransition (case_.status) Case.CLOSED & fromEitherM CaseInvalidStatus
              MC.updateStatusFlow searchCaseId Case.CLOSED
          )
      Left err -> logTagError "on_cancel req" $ "on_cancel error: " <> show err
    return Ack
