{-# LANGUAGE OverloadedLabels #-}

module Product.Cancel (cancel, onCancel) where

import App.Types
import Beckn.Types.APISuccess (APISuccess (Success))
import qualified Beckn.Types.Core.API.Cancel as API
import Beckn.Types.Core.Ack (AckResponse (..), Status (..), ack)
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Models.Case as MC
import qualified Models.ProductInstance as MPI
import qualified Storage.Queries.Organization as OQ
import Types.API.Cancel as Cancel
import Types.Error
import Utils.Common (validateContext)
import qualified Utils.Metrics as Metrics
import qualified Utils.Notifications as Notify

cancel :: Person.Person -> Cancel.CancelReq -> FlowHandler CancelRes
cancel person req = withFlowHandlerAPI $ do
  let entityType = req ^. #entityType
  case entityType of
    Cancel.CASE -> searchCancel person req
    Cancel.PRODUCT_INSTANCE -> cancelProductInstance person req

cancelProductInstance :: Person.Person -> CancelReq -> Flow CancelRes
cancelProductInstance person req = do
  let prodInstId = req ^. #entityId
  searchPI <- MPI.findById (Id prodInstId) -- TODO: Handle usecase where multiple productinstances exists for one product
  cs <- MC.findIdByPerson person (searchPI ^. #_caseId)
  orderPI <- MPI.findByParentIdType (searchPI ^. #_id) Case.RIDEORDER
  if isProductInstanceCancellable orderPI
    then sendCancelReq searchPI cs
    else throwError $ PIInvalidStatus "Cannot cancel with this ride"
  where
    sendCancelReq prodInst cs = do
      let txnId = getId $ cs ^. #_id
      let prodInstId = getId $ prodInst ^. #_id
      let cancelReqMessage = API.CancelReqMessage (API.CancellationOrder prodInstId Nothing)
      context <- buildContext "cancel" txnId Nothing Nothing
      organization <-
        OQ.findOrganizationById (Id $ prodInst ^. #_organizationId)
          >>= fromMaybeM OrgNotFound
      baseUrl <- organization ^. #_callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
      Gateway.cancel baseUrl (API.CancelReq context cancelReqMessage)
        >>= checkAckResponseError (ExternalAPIResponseError "cancel")
      return Success

searchCancel :: Person.Person -> CancelReq -> Flow CancelRes
searchCancel person req = do
  let caseId = req ^. #entityId
  case_ <- MC.findIdByPerson person (Id caseId)
  unless (isCaseCancellable case_) $ throwError (CaseInvalidStatus "Cannot cancel with this case")
  Metrics.incrementCaseCount Case.CLOSED Case.RIDESEARCH
  piList <- MPI.findAllByCaseId (case_ ^. #_id)
  traverse_ (`MPI.updateStatus` PI.CANCELLED) (PI._id <$> filter isProductInstanceCancellable piList)
  MC.updateStatus (case_ ^. #_id) Case.CLOSED
  return Success

isProductInstanceCancellable :: PI.ProductInstance -> Bool
isProductInstanceCancellable prodInst =
  isRight $ PI.validateStatusTransition (prodInst ^. #_status) PI.CANCELLED

isCaseCancellable :: Case.Case -> Bool
isCaseCancellable case_ =
  case case_ ^. #_status of
    Case.NEW -> True
    Case.CONFIRMED -> True
    _ -> False

onCancel :: Organization.Organization -> API.OnCancelReq -> FlowHandler API.OnCancelRes
onCancel _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "on_cancel" $ req ^. #context
    let context = req ^. #context
    case req ^. #contents of
      Right msg -> do
        let prodInstId = Id $ msg ^. #id
        -- TODO: Handle usecase where multiple productinstances exists for one product

        piList <- MPI.findAllByParentId prodInstId
        case piList of
          [] -> return ()
          s : _ -> do
            let orderPi = s
            -- TODO what if we update several PI but then get an error?
            -- wrap everything in a transaction
            -- or use updateMultiple
            MPI.updateStatus (PI._id orderPi) PI.CANCELLED
            MC.updateStatus (PI._caseId orderPi) Case.CLOSED
            return ()
        productInstance <- MPI.findById prodInstId
        MPI.updateStatus prodInstId PI.CANCELLED
        let caseId = productInstance ^. #_caseId
        -- notify customer
        Notify.notifyOnStatusUpdate productInstance PI.CANCELLED
        --
        arrPICase <- MPI.findAllByCaseId caseId
        let arrTerminalPI =
              filter
                ( \prodInst -> do
                    let status = prodInst ^. #_status
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
              MC.updateStatus caseId Case.CLOSED
          )
      Left err -> logTagError "on_cancel req" $ "on_cancel error: " <> show err
    return $ AckResponse context (ack ACK) Nothing
