{-# LANGUAGE OverloadedLabels #-}

module Product.Cancel where

import qualified Beckn.Types.API.Cancel as API
import Beckn.Types.App
import Beckn.Types.Common (IdObject (..))
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Products as Products
import Beckn.Utils.Common (mkAckResponse, mkAckResponse', mkNAckResponse, withFlowHandler)
import EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Models.Case as MC
import qualified Models.ProductInstance as MPI
import Servant.Client
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Products as Products
import Types.API.Cancel as Cancel
import qualified Utils.Notifications as Notify

cancel :: Person.Person -> CancelReq -> FlowHandler CancelRes
cancel person req = withFlowHandler $ do
  let entityType = req ^. #message . #entityType
  case entityType of
    Cancel.CASE -> cancelCase person req
    Cancel.PRODUCT_INSTANCE -> cancelProductInstance person req

cancelProductInstance :: Person.Person -> CancelReq -> L.Flow CancelRes
cancelProductInstance person req = do
  let prodInstId = req ^. #message . #entityId
  pi <- ProductInstance.findById (ProductInstanceId prodInstId) -- TODO: Handle usecase where multiple productinstances exists for one product
  Case.findIdByPerson person (pi ^. #_caseId)
  if isProductInstanceCancellable pi
    then sendCancelReq prodInstId
    else errResp (show (pi ^. #_status))
  where
    sendCancelReq prodInstId = do
      let context = req ^. #context
      let txnId = context ^. #transaction_id
      baseUrl <- Gateway.getBaseUrl
      eres <- Gateway.cancel baseUrl (API.CancelReq context (IdObject prodInstId))
      case eres of
        Left err -> mkAckResponse' txnId "cancel" ("Err: " <> show err)
        Right _ -> mkAckResponse txnId "cancel"
    errResp pStatus = do
      let txnId = req ^. #context . #transaction_id
      mkAckResponse' txnId "cancel" ("Err: Cannot CANCEL product in " <> pStatus <> " status")

cancelCase :: Person.Person -> CancelReq -> L.Flow CancelRes
cancelCase person req = do
  let caseId = req ^. #message . #entityId
  case_ <- Case.findIdByPerson person (CaseId caseId)
  if isCaseCancellable case_
    then do
      let context = req ^. #context
      let txnId = context ^. #transaction_id
      productInstances <- ProductInstance.findAllByCaseId (CaseId caseId)
      if null productInstances
        then do
          MC.updateStatus (CaseId caseId) Case.CLOSED
          mkAckResponse txnId "cancel"
        else do
          let cancelPIs = filter isProductInstanceCancellable productInstances
          baseUrl <- Gateway.getBaseUrl
          eres <- traverse (callCancelApi context baseUrl) cancelPIs
          case sequence eres of
            Left err -> mkAckResponse' txnId "cancel" ("Err: " <> show err)
            Right _ -> mkAckResponse txnId "cancel"
    else do
      let txnId = req ^. #context . #transaction_id
      mkAckResponse' txnId "cancel" ("Err: Cannot CANCEL case in " <> show (case_ ^. #_status) <> " status")
  where
    callCancelApi ::
      Context ->
      BaseUrl ->
      ProductInstance.ProductInstance ->
      Flow (Either Text ())
    callCancelApi context baseUrl pi = do
      let prodInstId = _getProductInstanceId $ pi ^. #_id
      Gateway.cancel baseUrl (API.CancelReq context (IdObject prodInstId))

isProductInstanceCancellable :: ProductInstance.ProductInstance -> Bool
isProductInstanceCancellable pi =
  case pi ^. #_status of
    ProductInstance.CONFIRMED -> True
    ProductInstance.VALID -> True
    ProductInstance.INSTOCK -> True
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
  let txnId = context ^. #transaction_id
  let prodInstId = ProductInstanceId $ req ^. #message . #id
  -- TODO: Handle usecase where multiple productinstances exists for one product
  productInstance <- ProductInstance.findById prodInstId
  MPI.updateStatus prodInstId ProductInstance.CANCELLED
  let caseId = productInstance ^. #_caseId
  -- notify customer
  case_ <- Case.findById caseId
  let personId = Case._requestor case_
  Notify.notifyOnProductCancelCb personId case_ prodInstId
  --
  arrPICase <- ProductInstance.findAllByCaseId caseId
  let arrTerminalPI =
        filter
          ( \pi -> do
              let status = pi ^. #_status
              status == ProductInstance.COMPLETED || status == ProductInstance.OUTOFSTOCK || status == ProductInstance.CANCELLED || status == ProductInstance.INVALID
          )
          arrPICase
  when
    (length arrTerminalPI == length arrPICase)
    (do
      MC.updateStatus caseId Case.CLOSED
      pure ()
    )
  mkAckResponse txnId "cancel"
