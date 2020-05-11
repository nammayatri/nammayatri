{-# LANGUAGE OverloadedLabels #-}

module Product.Confirm where

import Beckn.Types.API.Confirm
import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..), generateGUID)
import Beckn.Types.Core.Ack
import Beckn.Types.Mobility.Service
import qualified Beckn.Types.Storage.Products as SProducts
import Beckn.Utils.Common (withFlowHandler)
import Epass.Utils.Extra
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Gateway.Flow as Gateway
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Products as QProducts
import Types.App
import Utils.Routes

confirm :: Text -> Text -> FlowHandler AckResponse
confirm caseId productId = withFlowHandler $ do
  lt <- getCurrentTimeUTC
  currentCase <- QCase.findById $ CaseId caseId
  product <- QProducts.findById $ ProductsId productId
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

onConfirm :: OnConfirmReq -> FlowHandler OnConfirmRes
onConfirm req = withFlowHandler $ do
  eres <- traverse (flip QProducts.updateStatus SProducts.CONFIRMED . ProductsId) (req ^. #message ^. #_selected_items)
  let ack =
        case sequence eres of
          Left err -> Ack "on_confirm" ("Err: " <> show err)
          Right _ -> Ack "on_confirm" "Ok"
  return $ OnConfirmRes (req ^. #context) ack
