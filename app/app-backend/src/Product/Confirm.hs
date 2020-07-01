{-# LANGUAGE OverloadedLabels #-}

module Product.Confirm where

import Beckn.Types.API.Confirm
import qualified Beckn.Types.API.Track as Track
import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..), generateGUID)
import Beckn.Types.Core.Ack
import Beckn.Types.Mobility.Service
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as SCP
import qualified Beckn.Types.Storage.Products as Products
import Beckn.Utils.Common (decodeFromText, encodeToText, withFlowHandler)
import Beckn.Utils.Extra (getCurrentTimeUTC)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Gateway.Flow as Gateway
import qualified Models.Case as MC
import qualified Models.ProductInstance as MCP
import qualified Models.Product as MP
import Servant
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as QCP
import qualified Storage.Queries.Products as Products
import qualified Types.API.Confirm as API
import Types.App
import qualified Types.ProductInfo as Products
import Utils.Common (verifyToken)
import qualified Utils.Notifications as Notify
import Utils.Routes

confirm :: RegToken -> API.ConfirmReq -> FlowHandler AckResponse
confirm regToken API.ConfirmReq {..} = withFlowHandler $ do
  verifyToken regToken
  lt <- getCurrentTimeUTC
  case_ <- QCase.findById $ CaseId caseId
  when ((case_ ^. #_validTill) < lt)
    $ L.throwException
    $ err400 {errBody = "Case has expired"}
  productInstance <- QCP.findByCaseAndProductId (CaseId caseId) (ProductsId productId)
  transactionId <- L.generateGUID
  context <- buildContext "confirm" caseId
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
  -- TODO: Verify api key here
  L.logInfo "on_confirm req" (show req)
  let selectedItems = req ^. #message ^. #_selected_items
      trip = req ^. #message ^. #_trip
      caseId = CaseId $ req ^. #context ^. #transaction_id
  ack <-
    case length selectedItems of
      0 -> return $ Ack "on_confirm" "Ok"
      1 -> do
        let pid = ProductsId (head selectedItems)
            tracker = (flip Track.Tracker Nothing) <$> trip
        prd <- Products.findById pid
        let mprdInfo = decodeFromText =<< (prd ^. #_info)
        let uInfo = (\info -> info {Products._tracker = tracker}) <$> mprdInfo
        let uPrd =
              prd
                { Products._info = encodeToText <$> uInfo
                }
        productInstance <- QCP.findByProductId pid -- TODO: can have multiple cases linked, fix this
        MCP.updateStatus pid SCP.CONFIRMED
        MC.updateStatus (SCP._caseId productInstance) Case.INPROGRESS
        MP.updateMultiple pid uPrd
        MC.updateStatus caseId Case.INPROGRESS
        return $ Ack "on_confirm" "Ok"
      _ -> L.throwException $ err400 {errBody = "Cannot select more than one product."}
  return $ OnConfirmRes (req ^. #context) ack
