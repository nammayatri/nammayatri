{-# LANGUAGE OverloadedLabels #-}

module Product.Cancel where

import qualified Beckn.Types.API.Cancel as API
import Beckn.Types.App
import Beckn.Types.Common (IdObject (..))
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseProduct
import qualified Beckn.Types.Storage.Products as Products
import Beckn.Utils.Common (mkAckResponse, mkAckResponse', withFlowHandler)
import EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.CaseProduct as CaseProduct
import qualified Storage.Queries.Products as Products
import Types.API.Cancel as Cancel
import Utils.Common (verifyToken)

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
  cp <- CaseProduct.findByProductId (ProductsId productId) -- TODO: Handle usecase where multiple caseproducts exists for one product
  case isCaseProductCancellable cp of
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
      productInstances <- CaseProduct.findAllByCaseId (CaseId caseId)
      if null productInstances
        then do
          Case.updateStatus (CaseId caseId) Case.CLOSED
          mkAckResponse txnId "cancel"
        else do
          let cancelCPs = filter isCaseProductCancellable productInstances
          baseUrl <- Gateway.getBaseUrl
          eres <- traverse (callCancelApi context baseUrl) cancelCPs
          case sequence eres of
            Left err -> mkAckResponse' txnId "cancel" ("Err: " <> show err)
            Right _ -> mkAckResponse txnId "cancel"
  where
    callCancelApi context baseUrl cp = do
      let productId = _getProductsId $ cp ^. #_productId
      Gateway.cancel baseUrl (API.CancelReq context (IdObject productId))

isCaseProductCancellable :: CaseProduct.CaseProduct -> Bool
isCaseProductCancellable cp =
  case cp ^. #_status of
    CaseProduct.CONFIRMED -> True
    CaseProduct.VALID -> True
    CaseProduct.INSTOCK -> True
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
  cpProducts <- CaseProduct.findByProductId productId -- TODO: Handle usecase where multiple caseproducts exists for one product
  CaseProduct.updateStatus productId CaseProduct.CANCELLED
  let caseId = cpProducts ^. #_caseId
  arrCPCase <- CaseProduct.findAllByCaseId caseId
  let arrTerminalCP =
        filter
          ( \cp -> do
              let status = cp ^. #_status
              status == CaseProduct.COMPLETED || status == CaseProduct.OUTOFSTOCK || status == CaseProduct.CANCELLED || status == CaseProduct.INVALID
          )
          arrCPCase
  if length arrTerminalCP == length arrCPCase
    then Case.updateStatus caseId Case.CLOSED
    else return ()
  mkAckResponse txnId "cancel"
