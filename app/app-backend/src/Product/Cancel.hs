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
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import Servant.Client
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.ProductInstance as QPI
import Types.API.Cancel as Cancel
import qualified Utils.Notifications as Notify

cancel :: Person.Person -> CancelReq -> FlowHandler CancelRes
cancel person req = withFlowHandler $ do
  let entityType = req ^. #message . #entityType
  case entityType of
    Cancel.CASE -> cancelCase person req
    Cancel.PRODUCT_INSTANCE -> cancelProductInstance person req

cancelProductInstance :: Person.Person -> CancelReq -> Flow CancelRes
cancelProductInstance person req = do
  let prodInstId = req ^. #message . #entityId
  pi <- QPI.findById (ProductInstanceId prodInstId) -- TODO: Handle usecase where multiple productinstances exists for one product
  Case.findIdByPerson person (pi ^. #_caseId)
  if isProductInstanceCancellable pi
    then sendCancelReq prodInstId
    else errResp (show (pi ^. #_status))
  where
    sendCancelReq prodInstId = do
      let context = req ^. #context
      let txnId = context ^. #_transaction_id
      baseUrl <- Gateway.getBaseUrl
      let cancelReqMessage = API.CancelReqMessage (API.Cancellation txnId Nothing) (API.CancellationOrderId prodInstId)
      eres <- Gateway.cancel baseUrl (API.CancelReq context cancelReqMessage)
      case eres of
        Left err -> mkAckResponse' txnId "cancel" ("Err: " <> show err)
        Right _ -> mkAckResponse txnId "cancel"
    errResp pStatus = do
      let txnId = req ^. #context . #_transaction_id
      mkAckResponse' txnId "cancel" ("Err: Cannot CANCEL product in " <> pStatus <> " status")

cancelCase :: Person.Person -> CancelReq -> Flow CancelRes
cancelCase person req = do
  let caseId = req ^. #message . #entityId
  case_ <- Case.findIdByPerson person (CaseId caseId)
  if isCaseCancellable case_
    then do
      let context = req ^. #context
      let txnId = context ^. #_transaction_id
      productInstances <- QPI.findAllByCaseId (CaseId caseId)
      if null productInstances
        then do
          Case.updateStatus (CaseId caseId) Case.CLOSED
          mkAckResponse txnId "cancel"
        else do
          let cancelPIs = filter isProductInstanceCancellable productInstances
          baseUrl <- Gateway.getBaseUrl
          eres <- traverse (callCancelApi context baseUrl) cancelPIs
          case sequence eres of
            Left err -> mkAckResponse' txnId "cancel" ("Err: " <> show err)
            Right _ -> mkAckResponse txnId "cancel"
    else do
      let txnId = req ^. #context . #_transaction_id
      mkAckResponse' txnId "cancel" ("Err: Cannot CANCEL case in " <> show (case_ ^. #_status) <> " status")
  where
    callCancelApi ::
      Context ->
      BaseUrl ->
      PI.ProductInstance ->
      Flow (Either Text ())
    callCancelApi context baseUrl pi = do
      let txnId = context ^. #_transaction_id
      let prodInstId = _getProductInstanceId $ pi ^. #_id
      let cancelReqMessage = API.CancelReqMessage (API.Cancellation txnId Nothing) (API.CancellationOrderId prodInstId)
      Gateway.cancel baseUrl (API.CancelReq context cancelReqMessage)

isProductInstanceCancellable :: PI.ProductInstance -> Bool
isProductInstanceCancellable pi =
  case pi ^. #_status of
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
  let prodInstId = ProductInstanceId $ req ^. #message . #id
  -- TODO: Handle usecase where multiple productinstances exists for one product

  piList <- QPI.findAllByParentId (Just prodInstId)
  case piList of
    [] -> return ()
    s : _ -> do
      let orderPi = s
      QPI.updateStatus (PI._id orderPi) PI.CANCELLED
      return ()
  productInstance <- QPI.findById prodInstId
  QPI.updateStatus prodInstId PI.CANCELLED
  let caseId = productInstance ^. #_caseId
  -- notify customer
  case_ <- Case.findById caseId
  Notify.notifyOnProductCancelCb productInstance
  --
  arrPICase <- QPI.findAllByCaseId caseId
  let arrTerminalPI =
        filter
          ( \pi -> do
              let status = pi ^. #_status
              status == PI.COMPLETED
                || status == PI.OUTOFSTOCK
                || status == PI.CANCELLED
                || status == PI.INVALID
          )
          arrPICase
  when
    (length arrTerminalPI == length arrPICase)
    (Case.updateStatus caseId Case.CLOSED)
  mkAckResponse txnId "cancel"
