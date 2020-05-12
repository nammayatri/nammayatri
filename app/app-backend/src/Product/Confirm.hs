{-# LANGUAGE OverloadedLabels #-}

module Product.Confirm where

import Beckn.Types.API.Confirm
import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..), generateGUID)
import Beckn.Types.Core.Ack
import Beckn.Types.Mobility.Service
import qualified Beckn.Types.Storage.CaseProduct as SCP
import qualified Beckn.Types.Storage.Products as SProducts
import Beckn.Utils.Common (withFlowHandler)
import Epass.Utils.Extra
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Gateway.Flow as Gateway
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.CaseProduct as QCP
import qualified Storage.Queries.Products as QProducts
import Types.App
import Utils.Common (verifyToken)
import Utils.Routes

confirm :: Maybe RegToken -> Text -> Text -> FlowHandler AckResponse
confirm regToken caseId productId = withFlowHandler $ do
  verifyToken regToken
  lt <- getCurrentTimeUTC
  caseProduct <- QCP.findByCaseAndProductId (CaseId caseId) (ProductsId productId)
  transactionId <- L.generateGUID
  context <- buildContext "confirm" transactionId
  let service = Service caseId Nothing [] [productId] Nothing [] Nothing Nothing [] Nothing
  baseUrl <- Gateway.getBaseUrl
  eres <- Gateway.confirm baseUrl (ConfirmReq context service)
  let ack =
        case eres of
          Left err -> Ack "confirm" ("Err: " <> show err)
          Right _ -> Ack "confirm" "Ok"
  return $ AckResponse context ack

onConfirm :: Maybe RegToken -> OnConfirmReq -> FlowHandler OnConfirmRes
onConfirm regToken req = withFlowHandler $ do
  -- TODO: Verify api key here
  let update pid =
        QProducts.updateStatus pid SProducts.CONFIRMED
          >>= either (pure . Left) (\_ -> QCP.updateStatus pid SCP.CONFIRMED)
  eres <- traverse (update . ProductsId) (req ^. #message ^. #_selected_items)
  let ack =
        case sequence eres of
          Left err -> Ack "on_confirm" ("Err: " <> show err)
          Right _ -> Ack "on_confirm" "Ok"
  return $ OnConfirmRes (req ^. #context) ack
