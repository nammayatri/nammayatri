{-# LANGUAGE OverloadedLabels #-}

module Utils.Notifications where

import Beckn.External.FCM.Flow
import Beckn.External.FCM.Types as FCM
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Types.Storage.RegistrationToken as RegToken
import Control.Lens.Prism (_Just)
import qualified Data.Text as T
import EulerHS.Prelude
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.Person as Person
import Types.Metrics
import Types.ProductInfo as ProductInfo
import Utils.Common (decodeFromText, fromMaybeM, showTimeIst)

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
notifyOnStatusUpdate ::
  ( DBFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  ProductInstance ->
  ProductInstanceStatus ->
  m ()
notifyOnStatusUpdate prodInst piStatus =
  when (prodInst.status /= piStatus) $ do
    c <- Case.findById (prodInst.caseId) >>= fromMaybeM CaseNotFound
    let mpersonId = Case.requestor c
        productInstanceId = prodInst.id
        minfo :: (Maybe ProductInfo) = decodeFromText =<< prodInst.info
    case (mpersonId, minfo) of
      (Just personId, Just info) -> do
        person <- Person.findById $ Id personId
        case person of
          Just p -> case piStatus of
            ProductInstance.CANCELLED -> do
              let notificationData =
                    FCM.FCMAndroidData
                      { fcmNotificationType = FCM.CANCELLED_PRODUCT,
                        fcmShowNotification = FCM.SHOW,
                        fcmEntityType = FCM.Product,
                        fcmEntityIds = show $ getId productInstanceId,
                        fcmNotificationJSON = createAndroidNotification title body FCM.CANCELLED_PRODUCT
                      }
                  title = FCMNotificationTitle $ T.pack "Ride cancelled!"
                  providerName = info ^. #provider . _Just . #name . _Just
                  body =
                    FCMNotificationBody $
                      unwords
                        [ providerName,
                          "had to cancel your ride for",
                          showTimeIst (Case.startTime c) <> ".",
                          "Check the app for more details."
                        ]
              notifyPerson notificationData p
            ProductInstance.INPROGRESS -> do
              let notificationData =
                    FCM.FCMAndroidData
                      { fcmNotificationType = FCM.TRIP_STARTED,
                        fcmShowNotification = FCM.SHOW,
                        fcmEntityType = FCM.Product,
                        fcmEntityIds = show $ getId productInstanceId,
                        fcmNotificationJSON = createAndroidNotification title body FCM.TRIP_STARTED
                      }
                  title = FCMNotificationTitle $ T.pack "Trip started!"
                  driverName = info ^. #tracker . _Just . #trip . #driver . _Just . #name
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
                      { fcmNotificationType = FCM.TRIP_FINISHED,
                        fcmShowNotification = FCM.SHOW,
                        fcmEntityType = FCM.Product,
                        fcmEntityIds = show $ getId productInstanceId,
                        fcmNotificationJSON = createAndroidNotification title body FCM.TRIP_FINISHED
                      }
                  title = FCMNotificationTitle $ T.pack "Trip finished!"
                  driverName = info ^. #tracker . _Just . #trip . #driver . _Just . #name
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
                      { fcmNotificationType = FCM.DRIVER_UNASSIGNED,
                        fcmShowNotification = FCM.SHOW,
                        fcmEntityType = FCM.Product,
                        fcmEntityIds = show $ getId productInstanceId,
                        fcmNotificationJSON = createAndroidNotification title body FCM.DRIVER_UNASSIGNED
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
                      { fcmNotificationType = FCM.DRIVER_ASSIGNMENT,
                        fcmShowNotification = FCM.SHOW,
                        fcmEntityType = FCM.Product,
                        fcmEntityIds = show $ getId productInstanceId,
                        fcmNotificationJSON = createAndroidNotification title body FCM.DRIVER_ASSIGNMENT
                      }
                  title = FCMNotificationTitle $ T.pack "Driver assigned!"
                  driverName = info ^. #tracker . _Just . #trip . #driver . _Just . #name
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

notifyOnExpiration ::
  ( FCMFlow m r,
    DBFlow m r,
    CoreMetrics m
  ) =>
  Case ->
  m ()
notifyOnExpiration caseObj = do
  let caseId = Case.id caseObj
  let personId = Case.requestor caseObj
  if isJust personId
    then do
      person <- Person.findById $ Id (fromJust personId)
      case person of
        Just p -> do
          let notificationData =
                FCM.FCMAndroidData
                  { fcmNotificationType = FCM.EXPIRED_CASE,
                    fcmShowNotification = FCM.SHOW,
                    fcmEntityType = FCM.Case,
                    fcmEntityIds = show $ getId caseId,
                    fcmNotificationJSON = createAndroidNotification title body FCM.EXPIRED_CASE
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

notifyOnRegistration ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  RegistrationToken ->
  Person ->
  m ()
notifyOnRegistration regToken person =
  let tokenId = RegToken.id regToken
      notificationData =
        FCM.FCMAndroidData
          { fcmNotificationType = FCM.REGISTRATION_APPROVED,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Organization,
            fcmEntityIds = show tokenId,
            fcmNotificationJSON = createAndroidNotification title body FCM.REGISTRATION_APPROVED
          }
      title = FCMNotificationTitle $ T.pack "Registration Completed!"
      body =
        FCMNotificationBody $
          unwords
            [ "Welcome to Yatri.",
              "Click here to book your first ride with us."
            ]
   in notifyPerson notificationData person

notifyOnTrackCb ::
  ( FCMFlow m r,
    DBFlow m r,
    CoreMetrics m
  ) =>
  Maybe Text ->
  Tracker ->
  Case ->
  m ()
notifyOnTrackCb personId tracker c =
  if isJust personId
    then do
      let caseId = Case.id c
      mperson <- Person.findById $ Id (fromJust personId)
      case mperson of
        Just p -> do
          let trip = tracker.trip
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
                      showTimeIst (Case.startTime c) <> ".",
                      "You would be notified 15 mins before the scheduled pick up time."
                    ]
              notificationData =
                FCM.FCMAndroidData
                  { fcmNotificationType = FCM.TRACKING_CALLBACK,
                    fcmShowNotification = FCM.SHOW,
                    fcmEntityType = FCM.Case,
                    fcmEntityIds = show caseId,
                    fcmNotificationJSON = createAndroidNotification title body FCM.TRACKING_CALLBACK
                  }
          notifyPerson notificationData p
        _ -> pure ()
    else pure ()
