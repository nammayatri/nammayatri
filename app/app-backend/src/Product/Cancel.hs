{-# LANGUAGE OverloadedLabels #-}

module Product.Cancel (cancel, onCancel) where

import App.Types
import qualified Beckn.Types.Core.API.Cancel as API
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Utils.Common (fromMaybeM500, mkAckResponse, mkAckResponse', withFlowHandler)
import Beckn.Utils.Logging (Log (..))
import Data.Time (getCurrentTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Models.Case as MC
import qualified Models.ProductInstance as MPI
import qualified Storage.Queries.Organization as OQ
import Types.API.Cancel as Cancel
import Utils.Common (mkContext, validateContext)
import qualified Utils.Metrics as Metrics
import qualified Utils.Notifications as Notify

cancel :: Person.Person -> Cancel.CancelReq -> FlowHandler CancelRes
cancel person req = withFlowHandler $ do
  let entityType = req ^. #message . #entityType
  case entityType of
    Cancel.CASE -> searchCancel person req
    Cancel.PRODUCT_INSTANCE -> cancelProductInstance person req

cancelProductInstance :: Person.Person -> CancelReq -> Flow CancelRes
cancelProductInstance person req = do
  let prodInstId = req ^. #message . #entityId
  searchPI <- MPI.findById (Id prodInstId) -- TODO: Handle usecase where multiple productinstances exists for one product
  cs <- MC.findIdByPerson person (searchPI ^. #_caseId)
  orderPI <- MPI.findByParentIdType (Just $ searchPI ^. #_id) Case.RIDEORDER
  if isProductInstanceCancellable orderPI
    then sendCancelReq searchPI cs
    else errResp (show (orderPI ^. #_status)) cs
  where
    sendCancelReq prodInst cs = do
      let txnId = getId $ cs ^. #_id
      let prodInstId = getId $ prodInst ^. #_id
      currTime <- L.runIO getCurrentTime
      msgId <- L.generateGUID
      let cancelReqMessage = API.CancelReqMessage (API.CancellationOrder prodInstId Nothing)
          context = mkContext "cancel" txnId msgId currTime Nothing Nothing
      organization <-
        OQ.findOrganizationById (Id $ prodInst ^. #_organizationId)
          >>= fromMaybeM500 "INVALID_PROVIDER_ID"
      baseUrl <- organization ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
      eres <- Gateway.cancel baseUrl (API.CancelReq context cancelReqMessage)
      case eres of
        Left err -> mkAckResponse' txnId "cancel" ("Err: " <> show err)
        Right _ -> mkAckResponse txnId "cancel"
    errResp pStatus cs = do
      let txnId = getId $ cs ^. #_id
      mkAckResponse' txnId "cancel" ("Err: Cannot CANCEL product in " <> pStatus <> " status")

searchCancel :: Person.Person -> CancelReq -> Flow CancelRes
searchCancel person req = do
  let caseId = req ^. #message . #entityId
  let txnId = caseId
  case_ <- MC.findIdByPerson person (Id caseId)
  if isCaseCancellable case_
    then do
      Metrics.incrementCaseCount Case.CLOSED Case.RIDESEARCH
      piList <- MPI.findAllByCaseId (case_ ^. #_id)
      traverse_ (`MPI.updateStatus` PI.CANCELLED) (PI._id <$> filter isProductInstanceCancellable piList)
      MC.updateStatus (case_ ^. #_id) Case.CLOSED
      mkAckResponse txnId "cancel"
    else do
      mkAckResponse' txnId "cancel" ("Err: Cannot CANCEL case in " <> show (case_ ^. #_status) <> " status")

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
onCancel _org req = withFlowHandler $ do
  validateContext "on_cancel" $ req ^. #context
  let context = req ^. #context
  let txnId = context ^. #_transaction_id
  case req ^. #contents of
    Right msg -> do
      let prodInstId = Id $ msg ^. #id
      -- TODO: Handle usecase where multiple productinstances exists for one product

      piList <- MPI.findAllByParentId (Just prodInstId)
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
    Left err -> logError "on_cancel req" $ "on_cancel error: " <> show err
  mkAckResponse txnId "cancel"
