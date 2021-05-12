{-# LANGUAGE OverloadedLabels #-}

module Utils.Notifications where

import App.Types
import Beckn.External.FCM.Flow
import Beckn.External.FCM.Types as FCM
import Beckn.Types.Id
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Types.Storage.RegistrationToken as RegToken
import Beckn.Utils.Common (decodeFromText, showTimeIst)
import Control.Lens.Prism (_Just)
import qualified Data.Text as T
import EulerHS.Prelude
import qualified Models.Case as Case
import qualified Storage.Queries.Person as Person
import Types.ProductInfo as ProductInfo

-- Note:
-- When customer searches case is created in the BA, and search request is
-- sent to BP, which creates a case in the BP also. When someone responds to
-- that saying, they can offer this ride, onSearch is called to BP for each
-- of these and product/caseProduct is created. Then the customer would
-- confirm one of these product. Which would be basically choosing on of
-- the offers. Only these are cancellable in the BP, which needs to send
-- a notification in BA as a part of onCancel. Similary, when BA is cancelling,
-- cancel should send a notification to the provider who had the ride.
-- This will be basically cancelling the product/caseProduct. Here, when the
-- onCancel comes, it would be ideal not to send a notification in BA. But
-- it's also okay to send this in the first cut. The BA also has a case
-- cancellation flow, which basically cancels the case with or without products.
-- If the case with product is being cancelled, you have to send notification
-- in the BP for each product. Here it would be mostly one product again.
-- When case doesn't have any product, there is no notification.
notifyOnStatusUpdate :: ProductInstance -> ProductInstanceStatus -> Flow ()
notifyOnStatusUpdate prodInst piStatus =
  when (prodInst ^. #_status /= piStatus) $ do
    c <- Case.findById $ prodInst ^. #_caseId
    let mpersonId = Case._requestor c
        productInstanceId = prodInst ^. #_id
        minfo :: (Maybe ProductInfo) = decodeFromText =<< prodInst ^. #_info
    case (mpersonId, minfo) of
      (Just personId, Just info) -> do
        person <- Person.findById $ Id personId
        case person of
          Just p -> case piStatus of
            ProductInstance.CANCELLED -> do
              let notificationData =
                    FCM.FCMAndroidData
                      { _fcmNotificationType = FCM.CANCELLED_PRODUCT,
                        _fcmShowNotification = FCM.SHOW,
                        _fcmEntityType = FCM.Product,
                        _fcmEntityIds = show $ getId productInstanceId,
                        _fcmNotificationJSON = createAndroidNotification title body FCM.CANCELLED_PRODUCT
                      }
                  title = FCMNotificationTitle $ T.pack "Ride cancelled!"
                  providerName = info ^. #_provider . _Just . #name . _Just
                  body =
                    FCMNotificationBody $
                      unwords
                        [ providerName,
                          "had to cancel your ride for",
                          showTimeIst (Case._startTime c) <> ".",
                          "Check the app for more details."
                        ]
              notifyPerson notificationData p
            ProductInstance.INPROGRESS -> do
              let notificationData =
                    FCM.FCMAndroidData
                      { _fcmNotificationType = FCM.TRIP_STARTED,
                        _fcmShowNotification = FCM.SHOW,
                        _fcmEntityType = FCM.Product,
                        _fcmEntityIds = show $ getId productInstanceId,
                        _fcmNotificationJSON = createAndroidNotification title body FCM.TRIP_STARTED
                      }
                  title = FCMNotificationTitle $ T.pack "Trip started!"
                  driverName = info ^. #_tracker . _Just . #_trip . #driver . _Just . #name
                  body =
                    FCMNotificationBody $
                      unwords
                        [ driverName,
                          "has started your trip. Please enjoy the ride!"
                        ]
              notifyPerson notificationData p
            ProductInstance.COMPLETED -> do
              let notificationData =
                    FCM.FCMAndroidData
                      { _fcmNotificationType = FCM.TRIP_FINISHED,
                        _fcmShowNotification = FCM.SHOW,
                        _fcmEntityType = FCM.Product,
                        _fcmEntityIds = show $ getId productInstanceId,
                        _fcmNotificationJSON = createAndroidNotification title body FCM.TRIP_FINISHED
                      }
                  title = FCMNotificationTitle $ T.pack "Trip finished!"
                  driverName = info ^. #_tracker . _Just . #_trip . #driver . _Just . #name
                  body =
                    FCMNotificationBody $
                      unwords
                        [ "Hope you enjoyed your trip with",
                          driverName
                        ]
              notifyPerson notificationData p
            ProductInstance.TRIP_REASSIGNMENT -> do
              let notificationData =
                    FCM.FCMAndroidData
                      { _fcmNotificationType = FCM.DRIVER_UNASSIGNED,
                        _fcmShowNotification = FCM.SHOW,
                        _fcmEntityType = FCM.Product,
                        _fcmEntityIds = show $ getId productInstanceId,
                        _fcmNotificationJSON = createAndroidNotification title body FCM.DRIVER_UNASSIGNED
                      }
                  title = FCMNotificationTitle $ T.pack "Assigning another driver for the ride!"
                  body =
                    FCMNotificationBody $
                      unwords
                        ["Current driver has cancelled the ride. Looking for other drivers..."]
              notifyPerson notificationData p
            ProductInstance.TRIP_ASSIGNED -> do
              let notificationData =
                    FCM.FCMAndroidData
                      { _fcmNotificationType = FCM.DRIVER_ASSIGNMENT,
                        _fcmShowNotification = FCM.SHOW,
                        _fcmEntityType = FCM.Product,
                        _fcmEntityIds = show $ getId productInstanceId,
                        _fcmNotificationJSON = createAndroidNotification title body FCM.DRIVER_ASSIGNMENT
                      }
                  title = FCMNotificationTitle $ T.pack "Driver assigned!"
                  driverName = info ^. #_tracker . _Just . #_trip . #driver . _Just . #name
                  body =
                    FCMNotificationBody $
                      unwords
                        [ driverName,
                          "will be your driver for this trip."
                        ]
              notifyPerson notificationData p
            _ -> pure ()
          _ -> pure ()
      _ -> pure ()

notifyOnExpiration :: Case -> Flow ()
notifyOnExpiration caseObj = do
  let caseId = Case._id caseObj
  let personId = Case._requestor caseObj
  if isJust personId
    then do
      person <- Person.findById $ Id (fromJust personId)
      case person of
        Just p -> do
          let notificationData =
                FCM.FCMAndroidData
                  { _fcmNotificationType = FCM.EXPIRED_CASE,
                    _fcmShowNotification = FCM.SHOW,
                    _fcmEntityType = FCM.Case,
                    _fcmEntityIds = show $ getId caseId,
                    _fcmNotificationJSON = createAndroidNotification title body FCM.EXPIRED_CASE
                  }
              title = FCMNotificationTitle $ T.pack "Ride expired!"
              body =
                FCMNotificationBody $
                  unwords
                    [ "Your ride has expired as you did not confirm any offer.",
                      "Please book again to continue."
                    ]
          notifyPerson notificationData p
        _ -> pure ()
    else pure ()

notifyOnRegistration :: RegistrationToken -> Person -> Flow ()
notifyOnRegistration regToken person =
  let tokenId = RegToken._id regToken
      notificationData =
        FCM.FCMAndroidData
          { _fcmNotificationType = FCM.REGISTRATION_APPROVED,
            _fcmShowNotification = FCM.SHOW,
            _fcmEntityType = FCM.Organization,
            _fcmEntityIds = show tokenId,
            _fcmNotificationJSON = createAndroidNotification title body FCM.REGISTRATION_APPROVED
          }
      title = FCMNotificationTitle $ T.pack "Registration Completed!"
      body =
        FCMNotificationBody $
          unwords
            [ "Welcome to Yatri.",
              "Click here to book your first ride with us."
            ]
   in notifyPerson notificationData person

notifyOnTrackCb :: Maybe Text -> Tracker -> Case -> Flow ()
notifyOnTrackCb personId tracker c =
  if isJust personId
    then do
      let caseId = Case._id c
      mperson <- Person.findById $ Id (fromJust personId)
      case mperson of
        Just p -> do
          let trip = tracker ^. #_trip
              regNumber =
                trip ^. #vehicle . _Just . #registrationNumber . _Just
              model =
                fromMaybe "unknown" $ trip ^. #vehicle . _Just . #model
              driverName =
                trip ^. #driver . _Just . #name
              title = FCMNotificationTitle $ T.pack "Ride details updated!"
              body =
                FCMNotificationBody $
                  unwords
                    [ driverName,
                      "will be arriving in a",
                      model,
                      "(" <> regNumber <> "),",
                      "to pick you up on",
                      showTimeIst (Case._startTime c) <> ".",
                      "You would be notified 15 mins before the scheduled pick up time."
                    ]
              notificationData =
                FCM.FCMAndroidData
                  { _fcmNotificationType = FCM.TRACKING_CALLBACK,
                    _fcmShowNotification = FCM.SHOW,
                    _fcmEntityType = FCM.Case,
                    _fcmEntityIds = show caseId,
                    _fcmNotificationJSON = createAndroidNotification title body FCM.TRACKING_CALLBACK
                  }
          notifyPerson notificationData p
        _ -> pure ()
    else pure ()
