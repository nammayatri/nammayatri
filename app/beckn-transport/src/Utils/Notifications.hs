{-# LANGUAGE OverloadedLabels #-}

module Utils.Notifications where

import Beckn.External.FCM.Flow
import Beckn.External.FCM.Types as FCM
import Beckn.Types.App
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.CaseProduct as CaseProduct
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.Products as Products
import Beckn.Types.Storage.RegistrationToken as RegToken
import Beckn.Utils.Common (showTimeIst)
import qualified Data.Text as T
import Data.Time
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.CaseProduct as CaseProduct
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Products as Products

-- | Send FCM "search" notification to provider admins
notifyTransportersOnSearch :: Case -> [Person] -> L.Flow ()
notifyTransportersOnSearch c =
  traverse_ (notifyPerson title body notificationData)
  where
    notificationData =
      FCMData SEARCH_REQUEST SHOW FCM.Organization $
        show (_getCaseId $ c ^. #_id)
    title = FCMNotificationTitle $ T.pack "New ride request!"
    body =
      FCMNotificationBody $
        unwords
          [ "You have a new ride request for",
            (showTimeIst $ Case._startTime c) <> ".",
            "Check the app to accept or decline and see more details."
          ]

-- | Send FCM "confirm" notification to provider admins
notifyTransportersOnConfirm :: Case -> [Person] -> L.Flow ()
notifyTransportersOnConfirm c =
  traverse_ (notifyPerson title body notificationData)
  where
    notificationData =
      FCMData CONFIRM_REQUEST SHOW FCM.Organization $
        show (_getCaseId $ c ^. #_id)
    title = FCMNotificationTitle $ T.pack "Customer has confirmed the ride!"
    body =
      FCMNotificationBody $
        unwords
          [ "The ride for",
            showTimeIst $ Case._startTime c,
            "is confirmed. Check the app to assign driver details."
          ]

-- | Send FCM "cancel" notification to provider admins
notifyTransportersOnCancel :: Case -> T.Text -> [Person] -> L.Flow ()
notifyTransportersOnCancel c productId =
  traverse_ (notifyPerson title body notificationData)
  where
    notificationData =
      FCMData CANCELLED_PRODUCT SHOW FCM.Product $
        show productId
    title = FCMNotificationTitle $ T.pack "Ride cancelled!"
    body =
      FCMNotificationBody $
        unwords
          [ "The ride scheduled for",
            (showTimeIst $ Case._startTime c) <> ",",
            "has been cancelled. Check the app for more details."
          ]

notifyOnRegistration :: RegistrationToken -> Person -> L.Flow ()
notifyOnRegistration regToken =
  notifyPerson title body notificationData
  where
    tokenId = RegToken._id regToken
    notificationData =
      FCMData REGISTRATION_APPROVED SHOW FCM.Organization $
        show tokenId
    title = FCMNotificationTitle $ T.pack "Registration Completed!"
    body = FCMNotificationBody $ T.pack "You can now start accepting rides!"

notifyTransporterOnExpiration :: Case -> [Person] -> L.Flow ()
notifyTransporterOnExpiration c =
  traverse_ (notifyPerson title body notificationData)
  where
    notificationData =
      FCMData EXPIRED_CASE SHOW FCM.Case $
        show (_getCaseId $ c ^. #_id)
    title = FCMNotificationTitle $ T.pack "Ride expired!"
    body =
      FCMNotificationBody $
        unwords
          [ "The ride request for",
            (showTimeIst $ Case._startTime c) <> ",",
            "has expired as the customer failed to confirm.",
            "You can view more details in the app."
          ]
