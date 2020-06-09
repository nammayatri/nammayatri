{-# LANGUAGE OverloadedLabels #-}

module Product.Confirm where

import Beckn.External.FCM.Flow as FCM
import Beckn.External.FCM.Types
import Beckn.Types.API.Confirm
import qualified Beckn.Types.API.Track as Track
import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..), generateGUID)
import Beckn.Types.Core.Ack
import Beckn.Types.Mobility.Service
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as SCP
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
import Servant
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.CaseProduct as QCP
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Products as Products
import qualified Types.API.Confirm as API
import Types.App
import qualified Types.ProductInfo as Products
import Utils.Common (verifyToken)
import Utils.Routes

confirm :: Maybe RegToken -> API.ConfirmReq -> FlowHandler AckResponse
confirm regToken API.ConfirmReq {..} = withFlowHandler $ do
  verifyToken regToken
  lt <- getCurrentTimeUTC
  caseProduct <- QCP.findByCaseAndProductId (CaseId caseId) (ProductsId productId)
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

onConfirm :: Maybe RegToken -> OnConfirmReq -> FlowHandler OnConfirmRes
onConfirm regToken req = withFlowHandler $ do
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
                { Products._status = Products.CONFIRMED,
                  Products._info = encodeToText <$> uInfo
                }
        QCP.updateStatus pid Products.CONFIRMED
        Products.updateMultiple (_getProductsId pid) uPrd
        QCase.updateStatus caseId Case.INPROGRESS
        case_ <- QCase.findById caseId
        let personId = Case._requestor case_
        notifyOnConfirmCb personId caseId
        return $ Ack "on_confirm" "Ok"
      _ -> L.throwException $ err400 {errBody = "Cannot select more than one product."}
  return $ OnConfirmRes (req ^. #context) ack

notifyOnConfirmCb :: Maybe Text -> CaseId -> L.Flow ()
notifyOnConfirmCb personId caseId =
  if isJust personId
    then do
      person <- Person.findById $ PersonId (fromJust personId)
      case person of
        Just p -> do
          let notificationData =
                FCMData
                  { _fcmNotificationType = "CONFIRM_CALLBACK",
                    _fcmShowNotification = "true",
                    _fcmEntityIds = show $ _getCaseId caseId,
                    _fcmEntityType = "Case"
                  }
              title = "New ride options available!"
              body = T.pack "You have a new reply for your ride request! Head to the beckn app for details."
          FCM.notifyPerson title body notificationData p
          pure ()
        _ -> pure ()
    else pure ()
