{-# LANGUAGE OverloadedLabels #-}

module Product.Cancel where

import qualified Beckn.Types.API.Cancel as API
import Beckn.Types.App
import Beckn.Types.Common (IdObject (..))
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Products as Products
import Beckn.Utils.Common (mkAckResponse, mkAckResponse', withFlowHandler)
import EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
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
    Cancel.PRODUCT -> cancelProduct person req

cancelProduct :: Person.Person -> CancelReq -> L.Flow CancelRes
cancelProduct person req = do
  let productId = req ^. #message . #entityId
  cp <- ProductInstance.findByProductId (ProductsId productId) -- TODO: Handle usecase where multiple productinstances exists for one product
  Case.findIdByPerson person (cp ^. #_caseId)
  if isProductInstanceCancellable cp
    then sendCancelReq productId
    else errResp (show (cp ^. #_status))
  where
    sendCancelReq productId = do
      let context = req ^. #context
      let txnId = context ^. #transaction_id
      baseUrl <- Gateway.getBaseUrl
      eres <- Gateway.cancel baseUrl (API.CancelReq context (IdObject productId))
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
          Case.updateStatus (CaseId caseId) Case.CLOSED
          mkAckResponse txnId "cancel"
        else do
          let cancelCPs = filter isProductInstanceCancellable productInstances
          baseUrl <- Gateway.getBaseUrl
          eres <- traverse (callCancelApi context baseUrl) cancelCPs
          case sequence eres of
            Left err -> mkAckResponse' txnId "cancel" ("Err: " <> show err)
            Right _ -> mkAckResponse txnId "cancel"
    else do
      let txnId = req ^. #context . #transaction_id
      mkAckResponse' txnId "cancel" ("Err: Cannot CANCEL case in " <> show (case_ ^. #_status) <> " status")
  where
    callCancelApi context baseUrl cp = do
      let productId = _getProductsId $ cp ^. #_productId
      Gateway.cancel baseUrl (API.CancelReq context (IdObject productId))

isProductInstanceCancellable :: ProductInstance.ProductInstance -> Bool
isProductInstanceCancellable cp =
  case cp ^. #_status of
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
  let productId = ProductsId $ req ^. #message . #id
  cpProducts <- ProductInstance.findByProductId productId -- TODO: Handle usecase where multiple productinstances exists for one product
  ProductInstance.updateStatus productId ProductInstance.CANCELLED
  let caseId = cpProducts ^. #_caseId
  -- notify customer
  case_ <- Case.findById caseId
  let personId = Case._requestor case_
  Notify.notifyOnProductCancelCb personId case_ productId
  --
  arrCPCase <- ProductInstance.findAllByCaseId caseId
  let arrTerminalCP =
        filter
          ( \cp -> do
              let status = cp ^. #_status
              status == ProductInstance.COMPLETED || status == ProductInstance.OUTOFSTOCK || status == ProductInstance.CANCELLED || status == ProductInstance.INVALID
          )
          arrCPCase
  when
    (length arrTerminalCP == length arrCPCase)
    (Case.updateStatus caseId Case.CLOSED)

  mkAckResponse txnId "cancel"
