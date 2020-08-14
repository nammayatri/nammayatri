{-# LANGUAGE OverloadedLabels #-}

module Product.Cancel where

import App.Types
import qualified Beckn.Types.API.Cancel as API
import Beckn.Types.App
import Beckn.Types.Core.Context
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Utils.Common (mkAckResponse, mkAckResponse', withFlowHandler)
import Data.Time (getCurrentTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Models.Case as MC
import qualified Models.ProductInstance as MPI
import Servant.Client
import Types.API.Cancel as Cancel
import Utils.Common (mkContext)
import qualified Utils.Metrics as Metrics
import qualified Utils.Notifications as Notify

cancel :: Person.Person -> Cancel.CancelReq -> FlowHandler CancelRes
cancel person req = withFlowHandler $ do
  let entityType = req ^. #message . #entityType
  case entityType of
    Cancel.CASE -> cancelCase person req
    Cancel.PRODUCT_INSTANCE -> cancelProductInstance person req

cancelProductInstance :: Person.Person -> CancelReq -> Flow CancelRes
cancelProductInstance person req = do
  let prodInstId = req ^. #message . #entityId
  prodInst <- MPI.findById (ProductInstanceId prodInstId) -- TODO: Handle usecase where multiple productinstances exists for one product
  _ <- MC.findIdByPerson person (prodInst ^. #_caseId)
  if isProductInstanceCancellable prodInst
    then sendCancelReq prodInstId
    else errResp (show (prodInst ^. #_status))
  where
    sendCancelReq prodInstId = do
      let txnId = req ^. #transaction_id
      baseUrl <- Gateway.getProviderBaseUrl
      currTime <- L.runIO getCurrentTime
      let cancelReqMessage = API.CancelReqMessage (API.Cancellation txnId Nothing) (API.CancellationOrderId prodInstId)
          context = mkContext "cancel" txnId currTime Nothing
      eres <- Gateway.cancel baseUrl (API.CancelReq context cancelReqMessage)
      case eres of
        Left err -> mkAckResponse' txnId "cancel" ("Err: " <> show err)
        Right _ -> mkAckResponse txnId "cancel"
    errResp pStatus = do
      let txnId = req ^. #transaction_id
      mkAckResponse' txnId "cancel" ("Err: Cannot CANCEL product in " <> pStatus <> " status")

cancelCase :: Person.Person -> CancelReq -> Flow CancelRes
cancelCase person req = do
  let caseId = req ^. #message . #entityId
  case_ <- MC.findIdByPerson person (CaseId caseId)
  currTime <- L.runIO getCurrentTime
  if isCaseCancellable case_
    then do
      let txnId = req ^. #transaction_id
          context = mkContext "cancel" txnId currTime Nothing
      productInstances <- MPI.findAllByCaseId (CaseId caseId)
      if null productInstances
        then do
          Metrics.incrementCaseCount Case.CLOSED Case.RIDESEARCH
          MC.updateStatus (CaseId caseId) Case.CLOSED
          mkAckResponse txnId "cancel"
        else do
          let cancelPIs = filter isProductInstanceCancellable productInstances
          baseUrl <- Gateway.getProviderBaseUrl
          eres <- traverse (callCancelApi context baseUrl) cancelPIs
          case sequence eres of
            Left err -> mkAckResponse' txnId "cancel" ("Err: " <> show err)
            Right _ -> mkAckResponse txnId "cancel"
    else do
      let txnId = req ^. #transaction_id
      mkAckResponse' txnId "cancel" ("Err: Cannot CANCEL case in " <> show (case_ ^. #_status) <> " status")
  where
    callCancelApi ::
      Context ->
      BaseUrl ->
      PI.ProductInstance ->
      Flow (Either Text ())
    callCancelApi context baseUrl prodInst = do
      let txnId = context ^. #_transaction_id
      let prodInstId = _getProductInstanceId $ prodInst ^. #_id
      let cancelReqMessage = API.CancelReqMessage (API.Cancellation txnId Nothing) (API.CancellationOrderId prodInstId)
      Gateway.cancel baseUrl (API.CancelReq context cancelReqMessage)

isProductInstanceCancellable :: PI.ProductInstance -> Bool
isProductInstanceCancellable prodInst =
  case prodInst ^. #_status of
    PI.CONFIRMED -> True
    PI.VALID -> True
    PI.INSTOCK -> True
    _ -> False

isCaseCancellable :: Case.Case -> Bool
isCaseCancellable case_ =
  case case_ ^. #_status of
    Case.NEW -> True
    Case.CONFIRMED -> True
    _ -> False

onCancel :: API.OnCancelReq -> FlowHandler API.OnCancelRes
onCancel req = withFlowHandler $ do
  let context = req ^. #context
  let txnId = context ^. #_transaction_id
  case req ^. #contents of
    Right msg -> do
      let prodInstId = ProductInstanceId $ msg ^. #id
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
      Notify.notifyOnProductCancelCb productInstance
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
    Left err -> L.logError @Text "on_cancel req" $ "on_cancel error: " <> show err
  mkAckResponse txnId "cancel"

