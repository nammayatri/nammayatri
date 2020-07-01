{-# LANGUAGE OverloadedLabels #-}

module Product.Cancel where

import qualified Beckn.Types.API.Cancel as API
import Beckn.Types.App
import Beckn.Types.Common (IdObject (..))
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Products as Products
import Beckn.Utils.Common (mkAckResponse, mkAckResponse', mkNAckResponse, withFlowHandler)
import EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Models.Case as MC
import qualified Models.ProductInstance as MCP
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Products as Products
import Types.API.Cancel as Cancel
import Utils.Common (verifyToken)
import qualified Utils.Notifications as Notify

cancel :: RegToken -> CancelReq -> FlowHandler CancelRes
cancel regToken req = withFlowHandler $ do
  verifyToken regToken
  let entityType = req ^. #message ^. #entityType
  case entityType of
    Cancel.CASE -> cancelCase req
    Cancel.PRODUCT -> cancelProduct req

cancelProduct :: CancelReq -> L.Flow CancelRes
cancelProduct req = do
  let productId = req ^. #message ^. #entityId
  cp <- ProductInstance.findByProductId (ProductsId productId) -- TODO: Handle usecase where multiple productinstances exists for one product
  case isProductInstanceCancellable cp of
    True -> sendCancelReq productId
    False -> errResp (show (cp ^. #_status))
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
      let txnId = req ^. #context ^. #transaction_id
      mkAckResponse' txnId "cancel" ("Err: Cannot CANCEL product in " <> pStatus <> " status")

cancelCase :: CancelReq -> L.Flow CancelRes
cancelCase req = do
  let caseId = req ^. #message ^. #entityId
  case_ <- Case.findById (CaseId caseId)
  case isCaseCancellable case_ of
    False -> do
      let txnId = req ^. #context ^. #transaction_id
      mkAckResponse' txnId "cancel" ("Err: Cannot CANCEL case in " <> (show (case_ ^. #_status)) <> " status")
    True -> do
      let context = req ^. #context
      let txnId = context ^. #transaction_id
      productInstances <- ProductInstance.findAllByCaseId (CaseId caseId)
      if null productInstances
        then do
          MC.updateStatus (CaseId caseId) Case.CLOSED
          mkAckResponse txnId "cancel"
        else do
          let cancelCPs = filter isProductInstanceCancellable productInstances
          baseUrl <- Gateway.getBaseUrl
          eres <- traverse (callCancelApi context baseUrl) cancelCPs
          case sequence eres of
            Left err -> mkAckResponse' txnId "cancel" ("Err: " <> show err)
            Right _ -> mkAckResponse txnId "cancel"
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
  let productId = ProductsId (req ^. #message ^. #id)
  cpProducts <- ProductInstance.findByProductId productId -- TODO: Handle usecase where multiple productinstances exists for one product
  res <- MCP.updateStatus productId ProductInstance.CANCELLED
  case res of
    Left err -> mkNAckResponse txnId "cancel" $ show err
    Right _ -> do
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
      if length arrTerminalCP == length arrCPCase
        then do
          res <- MC.updateStatus caseId Case.CLOSED
          case res of
            Left err -> mkNAckResponse txnId "cancel" $ show err
            Right _ -> mkAckResponse txnId "cancel"
        else mkAckResponse txnId "cancel"
