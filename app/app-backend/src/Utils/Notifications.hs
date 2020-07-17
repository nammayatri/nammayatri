{-# LANGUAGE OverloadedLabels #-}

module Utils.Notifications where

import App.Types
import Beckn.External.FCM.Flow
import Beckn.External.FCM.Types as FCM
import Beckn.Types.App
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Types.Storage.RegistrationToken as RegToken
import Beckn.Utils.Common (decodeFromText, showTimeIst)
import Control.Lens.Prism (_Just)
import qualified Data.Text as T
import EulerHS.Prelude
import qualified Storage.Queries.Case as QC
import qualified Storage.Queries.Person as Person
import Types.ProductInfo as ProductInfo

-- @boazjohn:
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
notifyOnProductCancelCb :: ProductInstance -> Flow ()
notifyOnProductCancelCb pi = do
  c <- QC.findById $ pi ^. #_caseId
  let mpersonId = Case._requestor c
      productInstanceId = pi ^. #_id
      minfo :: (Maybe ProductInfo) = decodeFromText =<< pi ^. #_info
  case (mpersonId, minfo) of
    (Just personId, Just info) -> do
      person <- Person.findById $ PersonId personId
      case person of
        Just p -> do
          let notificationData =
                FCMData CANCELLED_PRODUCT SHOW FCM.Product $
                  show (_getProductInstanceId productInstanceId)
              title = FCMNotificationTitle $ T.pack "Ride cancelled!"
              providerName = info ^. #_provider . _Just . #_name
              body =
                FCMNotificationBody $
                  unwords
                    [ providerName,
                      "had to cancel your ride scehduled for",
                      showTimeIst (Case._startTime c) <> ".",
                      "Check the app for more details."
                    ]
          notifyPerson title body notificationData p
        _ -> pure ()
    _ -> pure ()

-- -- | Notification on confirmation callback
-- -- unused, left as a sample, can be removed later
-- notifyOnConfirmCb :: Maybe Text -> Case -> Maybe Tracker -> Flow ()
-- notifyOnConfirmCb personId c tracker =
--   if isJust personId
--     then do
--       person <- Person.findById $ PersonId (fromJust personId)
--       case person of
--         Just p -> do
--           let notificationData =
--                 FCMData CONFIRM_CALLBACK SHOW FCM.Case $
--                   show (_getCaseId $ c ^. #_id)
--               vehicleCategory = case tracker of
--                 Nothing -> "unknown"
--                 Just t ->
--                   fromMaybe "unknown" $ Case._udf1 c
--               title = FCMNotificationTitle $ T.pack "Congratulations!"
--               body =
--                 FCMNotificationBody $
--                   unwords
--                     [ "You have successfully booked a",
--                       vehicleCategory,
--                       "for",
--                       showTimeIst $ Case._startTime c,
--                       "with",
--                       -- driverName,
--                       ". Click here for details."
--                     ]
--           notifyPerson title body notificationData p
--         _ -> pure ()
--     else pure ()

notifyOnExpiration :: Case -> Flow ()
notifyOnExpiration caseObj = do
  let caseId = Case._id caseObj
  let personId = Case._requestor caseObj
  let startTime = Case._startTime caseObj
  if isJust personId
    then do
      person <- Person.findById $ PersonId (fromJust personId)
      case person of
        Just p -> do
          let notificationData =
                FCMData EXPIRED_CASE SHOW FCM.Case $
                  show (_getCaseId caseId)
              title = FCMNotificationTitle $ T.pack "Ride expired!"
              body =
                FCMNotificationBody $
                  unwords
                    [ "Your ride for",
                      showTimeIst startTime,
                      "has expired as there were no replies.",
                      "You can place a new request to get started again!"
                    ]
          notifyPerson title body notificationData p
        _ -> pure ()
    else pure ()

notifyOnRegistration :: RegistrationToken -> Person -> Flow ()
notifyOnRegistration regToken person =
  let tokenId = RegToken._id regToken
      notificationData =
        FCMData REGISTRATION_APPROVED SHOW FCM.Organization $
          show tokenId
      title = FCMNotificationTitle $ T.pack "Registration Completed!"
      body =
        FCMNotificationBody $
          unwords
            [ "Welcome to Beckn.",
              "Click here to book your first ride with us."
            ]
   in notifyPerson title body notificationData person

notifyOnTrackCb :: Maybe Text -> Tracker -> Case -> Flow ()
notifyOnTrackCb personId tracker c =
  if isJust personId
    then do
      let caseId = Case._id c
      mperson <- Person.findById $ PersonId (fromJust personId)
      case mperson of
        Just p -> do
          let notificationData =
                FCMData TRACKING_CALLBACK SHOW FCM.Case $
                  show caseId
              trip = tracker ^. #_trip
              regNumber =
                trip ^. #vehicle . _Just . #registration . _Just . #number
              model =
                fromMaybe "unknown model" $ trip ^. #vehicle . _Just . #model
              driverName =
                trip ^. #driver . #persona . _Just . #descriptor . #first_name
              title = FCMNotificationTitle $ T.pack "Ride details updated!"
              body =
                FCMNotificationBody $
                  unwords
                    [ driverName,
                      "will come in a",
                      model,
                      "(" <> regNumber <> "),",
                      "to pick you up on",
                      showTimeIst (Case._startTime c) <> ".",
                      "You would be notified 15 mins before the",
                      "scheduled pick up time."
                    ]
          notifyPerson title body notificationData p
        _ -> pure ()
    else pure ()

notifyOnSearchCb :: PersonId -> Case -> [ProductInstance] -> Flow ()
notifyOnSearchCb personId c productInstances = do
  let caseId = Case._id c
  mperson <- Person.findById personId
  case mperson of
    Just p -> do
      let notificationData =
            FCMData SEARCH_CALLBACK SHOW FCM.Case $
              show caseId
          title = FCMNotificationTitle $ T.pack "New ride options available!"
          body =
            FCMNotificationBody $
              if length productInstances == 1
                then
                  unwords
                    [ "You have a new reply for your ride request dated",
                      showTimeIst (Case._startTime c) <> "!",
                      "Check the app for details."
                    ]
                else
                  unwords
                    [ "You have",
                      show (length productInstances),
                      "new ride offers!",
                      "Check your options in the beckn app."
                    ]
      notifyPerson title body notificationData p
    _ -> pure ()
