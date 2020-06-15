{-# LANGUAGE OverloadedLabels #-}

module Product.TrackTrip where

import qualified Beckn.External.FCM.Flow as FCM
import qualified Beckn.External.FCM.Types as FCM
import Beckn.Types.API.Track
import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..), generateGUID)
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Person as Person
import Beckn.Types.Mobility.Driver as Driver
import Beckn.Types.Mobility.Tracking
import Beckn.Types.Mobility.Trip
import Beckn.Types.Mobility.Vehicle as Vehicle
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseProduct
import qualified Beckn.Types.Storage.Products as Products
import Beckn.Utils.Common (decodeFromText, encodeToText, withFlowHandler)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.CaseProduct as CaseProduct
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Products as Products
import Types.ProductInfo as ProductInfo
import Utils.Common (verifyToken)

track :: Maybe Text -> TrackTripReq -> FlowHandler TrackTripRes
track regToken req = withFlowHandler $ do
  verifyToken regToken
  let context = req ^. #context
      tripId = req ^. #message ^. #id
  prd <- Products.findById $ ProductsId tripId
  ack <-
    case decodeFromText =<< (prd ^. #_info) of
      Nothing -> return $ Ack "Error" "No product to track"
      Just (info :: ProductInfo) -> do
        case ProductInfo._tracker info of
          Nothing -> return $ Ack "Error" "No product to track"
          Just tracker -> do
            let gTripId = tracker ^. #trip ^. #id
            gatewayUrl <- Gateway.getBaseUrl
            eres <- Gateway.track gatewayUrl $ req & (#message . #id) .~ gTripId
            case eres of
              Left err -> return $ Ack "Error" (show err)
              Right _ -> return $ Ack "Successful" "Tracking initiated"
  return $ AckResponse context ack

track_cb :: Maybe Text -> OnTrackTripReq -> FlowHandler OnTrackTripRes
track_cb apiKey req = withFlowHandler $ do
  -- TODO: verify api key
  let context = req ^. #context
      tracking = req ^. #message
      caseId = CaseId $ req ^. #context ^. #transaction_id
  case_ <- Case.findById caseId
  cp <- CaseProduct.findAllByCaseId caseId
  let pids = map CaseProduct._productId cp
  products <- Products.findAllByIds pids
  let confirmedProducts = filter (\prd -> Products.CONFIRMED == Products._status prd) products
  res <-
    case length confirmedProducts of
      0 -> return $ Right ()
      1 -> do
        let product = head confirmedProducts
            personId = Case._requestor case_
        notifyOnTrackCb personId tracking caseId
        updateTracker product tracking
      _ -> return $ Left "Multiple products confirmed, ambiguous selection"
  case res of
    Left err -> return $ AckResponse context (Ack "Error" err)
    Right _ -> return $ AckResponse context (Ack "Successful" "Ok")

updateTracker :: Products.Products -> Tracker -> L.Flow (Either Text ())
updateTracker product tracker = do
  let minfo = decodeFromText =<< product ^. #_info
  let uInfo = (\info -> info {ProductInfo._tracker = Just tracker}) <$> minfo
  let updatedPrd =
        product {Products._info = Just $ encodeToText uInfo}
  Products.updateMultiple (_getProductsId $ product ^. #_id) updatedPrd
  return $ Right ()

notifyOnTrackCb :: Maybe Text -> Tracker -> CaseId -> L.Flow ()
notifyOnTrackCb personId tracker caseId =
  if isJust personId
    then do
      person <- Person.findById $ PersonId (fromJust personId)
      case person of
        Just p -> do
          let notificationData =
                FCM.FCMData
                  { _fcmNotificationType = FCM.TRACKING_CALLBACK,
                    _fcmShowNotification = FCM.SHOW,
                    _fcmEntityIds = show caseId,
                    _fcmEntityType = FCM.Case
                  }
              vehicle = tracker ^. #trip ^. #vehicle
              driver = tracker ^. #trip ^. #driver ^. #persona
              vehicle_type =
                maybe "no vehicle" (\x -> fromMaybe "unknown" (x ^. #category)) vehicle
              driver_name =
                maybe "unknown" (\x -> x ^. #descriptor ^. #first_name) driver
              title = FCM.FCMNotificationTitle $ T.pack "Ride details updated!"
              body =
                FCM.FCMNotificationBody $
                  "Driver: " <> driver_name <> ", vehicle type: " <> vehicle_type
          FCM.notifyPerson title body notificationData p
          pure ()
        _ -> pure ()
    else pure ()
