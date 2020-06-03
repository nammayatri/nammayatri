{-# LANGUAGE OverloadedLabels #-}

module Product.Cancel where

import Beckn.Types.API.Cancel
import Beckn.Types.App
import Beckn.Types.Common (IdObject (..))
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Products as Products
import Beckn.Utils.Common (mkAckResponse, mkAckResponse', withFlowHandler)
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.CaseProduct as CaseProduct
import qualified Storage.Queries.Products as Products
import Utils.Common (verifyToken)

cancel :: Maybe RegToken -> CancelReq -> FlowHandler CancelRes
cancel regToken req = withFlowHandler $ do
  verifyToken regToken
  let productId = req ^. #message ^. #id
  cp <- CaseProduct.findByProductId (ProductsId productId) -- TODO: Handle usecase where multiple caseproducts exists for one product
  case cp ^. #_status of
    Products.CONFIRMED -> sendCancelReq productId
    Products.VALID -> sendCancelReq productId
    Products.INPROGRESS -> sendCancelReq productId
    status -> errResp (show status)
  where
    sendCancelReq productId = do
      let context = req ^. #context
      let txnId = context ^. #transaction_id
      baseUrl <- Gateway.getBaseUrl
      eres <- Gateway.cancel baseUrl (CancelReq context (IdObject productId))
      case eres of
        Left err -> mkAckResponse' txnId "cancel" ("Err: " <> show err)
        Right _ -> mkAckResponse txnId "cancel"
    errResp pStatus = do
      let txnId = req ^. #context ^. #transaction_id
      mkAckResponse' txnId "cancel" ("Err: Cannot CANCEL product in " <> pStatus <> " status")

onCancel :: OnCancelReq -> FlowHandler OnCancelRes
onCancel req = withFlowHandler $ do
  let context = req ^. #context
  let txnId = context ^. #transaction_id
  let productId = ProductsId (req ^. #message ^. #id)

  Products.updateStatus productId Products.CANCELLED
  cpProducts <- CaseProduct.findByProductId productId -- TODO: Handle usecase where multiple caseproducts exists for one product
  CaseProduct.updateStatus productId Products.CANCELLED
  let caseId = cpProducts ^. #_caseId
  arrCPCase <- CaseProduct.findAllByCaseId caseId
  let arrTerminalCP =
        filter
          ( \cp -> do
              let status = cp ^. #_status
              status == Products.COMPLETED || status == Products.OUTOFSTOCK || status == Products.CANCELLED || status == Products.INVALID
          )
          arrCPCase
  if length arrTerminalCP == length arrCPCase
    then Case.updateStatus caseId Case.CLOSED
    else return ()

  mkAckResponse txnId "cancel"
