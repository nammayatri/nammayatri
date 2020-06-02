{-# LANGUAGE OverloadedLabels #-}

module Product.Cancel where

import Beckn.Types.API.Cancel
import Beckn.Types.App
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
  let context = req ^. #context
  let productId = req ^. #message ^. #_id
  cp <- CaseProduct.findByProductId (ProductsId productId) -- TODO: Handle usecase where multiple caseproducts exists for one product
  baseUrl <- Gateway.getBaseUrl
  eres <- Gateway.cancel baseUrl (CancelReq context productId)
  let ack =
        case eres of
          Left err -> Ack "confirm" ("Err: " <> show err)
          Right _ -> Ack "confirm" "Ok"

  return $ CancelRes context ack

onCancel :: OnCancelReq -> FlowHandler OnCancelRes
onCancel req = withFlowHandler $ do
  let context = req ^. #context
  let productId = ProductsId (req ^. #message ^. #_id)

  Products.updateStatus productId Products.CANCELLED
  cpProducts <- CaseProduct.findByProductId productId -- TODO: Handle usecase where multiple caseproducts exists for one product
  CaseProduct.updateStatus productId Products.CANCELLED
  let caseId = cpProducts ^. #_caseId
  cpCase <- CaseProduct.findAllByCaseId caseId

  if length cpCase == 1
    then Case.updateStatus caseId Case.CLOSED -- Only update case status if it has only one product
    else return ()

  return $ CancelRes context (Ack "cancel" "Ok")
