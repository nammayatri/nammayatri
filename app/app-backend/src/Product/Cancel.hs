{-# LANGUAGE OverloadedLabels #-}

module Product.Cancel where

import Beckn.Types.API.Cancel
import Beckn.Types.App
import Beckn.Types.Common (IdObject (..))
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Products as Products
import Beckn.Utils.Common (withFlowHandler)
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
    _ -> errResp
  where
    sendCancelReq productId = do
      let context = req ^. #context
      baseUrl <- Gateway.getBaseUrl
      eres <- Gateway.cancel baseUrl (CancelReq context (IdObject productId))
      let ack =
            case eres of
              Left err -> Ack "cancel" ("Err: " <> show err)
              Right _ -> Ack "cancel" "Ok"
      return $ CancelRes context ack
    errResp = do
      let context = req ^. #context
      let ack = Ack "cancel" ("Err: Cannot cancel ride")
      return $ CancelRes context ack

onCancel :: OnCancelReq -> FlowHandler OnCancelRes
onCancel req = withFlowHandler $ do
  let context = req ^. #context
  let productId = ProductsId (req ^. #message ^. #id)

  Products.updateStatus productId Products.CANCELLED
  cpProducts <- CaseProduct.findByProductId productId -- TODO: Handle usecase where multiple caseproducts exists for one product
  CaseProduct.updateStatus productId Products.CANCELLED
  let caseId = cpProducts ^. #_caseId
  cpCase <- CaseProduct.findAllByCaseId caseId

  if length cpCase == 1
    then Case.updateStatus caseId Case.CLOSED -- Only update case status if it has only one product
    else return ()

  return $ OnCancelRes context (Ack "cancel" "Ok")
