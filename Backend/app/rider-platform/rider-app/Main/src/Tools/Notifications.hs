{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Notifications where

import qualified Data.Text as T
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import Domain.Types.Estimate (Estimate)
import qualified Domain.Types.Estimate as DEst
import Domain.Types.Merchant
import Domain.Types.Person as Person
import Domain.Types.Quote (makeQuoteAPIEntity)
import qualified Domain.Types.Quote as DQuote
import Domain.Types.RegistrationToken as RegToken
import qualified Domain.Types.Ride as SRide
import Domain.Types.SearchRequest as SearchRequest
import EulerHS.Prelude
import qualified Kernel.External.FCM.Flow as FCM
import Kernel.External.FCM.Types as FCM
import Kernel.Storage.Esqueleto
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.SearchRequest as QSearchReq
import Tools.Metrics

getFCMConfig ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  m FCM.FCMConfig
getFCMConfig merchId = do
  fmap (.fcmConfig) $ QMerchant.findById merchId >>= fromMaybeM (MerchantNotFound merchId.getId)

notifyOnDriverOfferIncoming ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  Id Estimate ->
  [DQuote.Quote] ->
  Person.Person ->
  m ()
notifyOnDriverOfferIncoming estimateId quotes person = do
  config <- getFCMConfig person.merchantId

  let notificationData =
        FCM.FCMData
          { fcmNotificationType = FCM.DRIVER_QUOTE_INCOMING,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = estimateId.getId,
            fcmEntityData = map makeQuoteAPIEntity quotes,
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.DRIVER_QUOTE_INCOMING
          }
      title = FCMNotificationTitle "New driver offers incoming!"
      body =
        FCMNotificationBody $
          unwords
            [ "There are new driver offers!",
              "Check the app for details"
            ]
  FCM.notifyPerson config notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken

notifyOnRideAssigned ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnRideAssigned booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  config <- getFCMConfig person.merchantId
  let notificationData =
        FCM.FCMData
          { fcmNotificationType = FCM.DRIVER_ASSIGNMENT,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = getId rideId,
            fcmEntityData = (),
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.DRIVER_ASSIGNMENT
          }
      title = FCMNotificationTitle $ T.pack "Driver assigned!"
      body =
        FCMNotificationBody $
          unwords
            [ driverName,
              "will be your driver for this trip."
            ]
  FCM.notifyPerson config notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken

notifyOnRideStarted ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnRideStarted booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  config <- getFCMConfig person.merchantId
  let notificationData =
        FCM.FCMData
          { fcmNotificationType = FCM.TRIP_STARTED,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = getId rideId,
            fcmEntityData = (),
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.TRIP_STARTED
          }
      title = FCMNotificationTitle $ T.pack "Trip started!"
      body =
        FCMNotificationBody $
          unwords
            [ driverName,
              "has started your trip. Please enjoy the ride!"
            ]
  FCM.notifyPerson config notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken

notifyOnRideCompleted ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnRideCompleted booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
      totalFare = ride.totalFare
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  config <- getFCMConfig person.merchantId
  let notificationData =
        FCM.FCMData
          { fcmNotificationType = FCM.TRIP_FINISHED,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = getId rideId,
            fcmEntityData = (),
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.TRIP_FINISHED
          }
      title = FCMNotificationTitle $ T.pack "Trip finished!"
      body =
        FCMNotificationBody $
          unwords
            [ "Hope you enjoyed your trip with",
              driverName,
              "Total Fare " <> show (fromMaybe booking.estimatedFare totalFare)
            ]
  FCM.notifyPerson config notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken

notifyOnExpiration ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  SearchRequest ->
  m ()
notifyOnExpiration searchReq = do
  let searchRequestId = searchReq.id
  let personId = searchReq.riderId
  person <- Person.findById personId
  case person of
    Just p -> do
      let notificationData =
            FCM.FCMData
              { fcmNotificationType = FCM.EXPIRED_CASE,
                fcmShowNotification = FCM.SHOW,
                fcmEntityType = FCM.SearchRequest,
                fcmEntityIds = getId searchRequestId,
                fcmEntityData = (),
                fcmNotificationJSON = FCM.createAndroidNotification title body FCM.EXPIRED_CASE
              }
          title = FCMNotificationTitle $ T.pack "Ride expired!"
          body =
            FCMNotificationBody $
              unwords
                [ "Your ride has expired as you did not confirm any offer.",
                  "Please book again to continue."
                ]
      config <- getFCMConfig p.merchantId
      FCM.notifyPerson config notificationData $ FCM.FCMNotificationRecipient p.id.getId p.deviceToken
    _ -> pure ()

notifyOnRegistration ::
  ( HasCacheConfig r,
    CoreMetrics m,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
  RegistrationToken ->
  Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyOnRegistration regToken person mbDeviceToken = do
  config <- getFCMConfig person.merchantId
  let tokenId = RegToken.id regToken
      notificationData =
        FCM.FCMData
          { fcmNotificationType = FCM.REGISTRATION_APPROVED,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Merchant,
            fcmEntityIds = getId tokenId,
            fcmEntityData = (),
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.REGISTRATION_APPROVED
          }
      title = FCMNotificationTitle $ T.pack "Registration Completed!"
      body =
        FCMNotificationBody $
          unwords
            [ "Welcome to Yatri.",
              "Click here to book your first ride with us."
            ]
   in FCM.notifyPerson config notificationData $ FCM.FCMNotificationRecipient person.id.getId mbDeviceToken

notifyOnBookingCancelled ::
  ( HasCacheConfig r,
    CoreMetrics m,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
  SRB.Booking ->
  SBCR.CancellationSource ->
  m ()
notifyOnBookingCancelled booking cancellationSource = do
  person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  config <- getFCMConfig person.merchantId
  FCM.notifyPerson config (notificationData $ booking.providerName) $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken
  where
    notificationData orgName =
      FCM.FCMData
        { fcmNotificationType = FCM.CANCELLED_PRODUCT,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = getId booking.id,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title (body orgName) FCM.CANCELLED_PRODUCT
        }
    title = FCMNotificationTitle $ T.pack "Ride cancelled!"
    body orgName =
      FCMNotificationBody $ getCancellationText orgName
    getCancellationText orgName = case cancellationSource of
      SBCR.ByUser ->
        unwords
          [ "You have cancelled your ride for",
            showTimeIst (booking.startTime) <> ".",
            "Check the app for details."
          ]
      SBCR.ByMerchant ->
        unwords
          [ "\"" <> orgName <> "\" agency had to cancel the ride for",
            showTimeIst (booking.startTime) <> ".",
            "Please book again to get another ride."
          ]
      SBCR.ByDriver ->
        unwords
          [ "The driver had to cancel the ride for",
            showTimeIst (booking.startTime) <> ".",
            "Please book again to get another ride."
          ]
      SBCR.ByAllocator ->
        unwords
          [ "The ride for",
            showTimeIst (booking.startTime),
            "was cancelled as we could not find a driver.",
            "Please book again to get another ride."
          ]
      SBCR.ByApplication ->
        unwords
          [ "The ride for",
            showTimeIst (booking.startTime),
            "was cancelled because quote was not confirmed.",
            "Please book again to get another ride."
          ]

notifyOnBookingReallocated ::
  ( HasCacheConfig r,
    CoreMetrics m,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
  SRB.Booking ->
  m ()
notifyOnBookingReallocated booking = do
  person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  notificationData <- buildNotificationData
  config <- getFCMConfig person.merchantId
  FCM.notifyPerson config notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken
  where
    buildNotificationData = do
      body <- buildBody
      return $
        FCM.FCMData
          { fcmNotificationType = FCM.REALLOCATE_PRODUCT,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = getId booking.id,
            fcmEntityData = (),
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.REALLOCATE_PRODUCT
          }
    title = FCMNotificationTitle $ T.pack "Ride cancelled!"
    buildBody = do
      FCMNotificationBody <$> getReallocationText
    getReallocationText =
      return $
        unwords
          [ "The driver had to cancel the ride for",
            showTimeIst (booking.startTime) <> ".",
            "Please wait until we allocate other driver."
          ]

notifyOnEstimatedReallocated ::
  ( HasCacheConfig r,
    CoreMetrics m,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
  SRB.Booking ->
  Id DEst.Estimate ->
  m ()
notifyOnEstimatedReallocated booking estimateId = do
  person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  notificationData <- buildNotificationData
  config <- getFCMConfig person.merchantId
  FCM.notifyPerson config notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken
  where
    buildNotificationData = do
      body <- buildBody
      return $
        FCM.FCMData
          { fcmNotificationType = FCM.REALLOCATE_PRODUCT,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = getId estimateId,
            fcmEntityData = (),
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.REALLOCATE_PRODUCT
          }
    title = FCMNotificationTitle $ T.pack "Ride cancelled!"
    buildBody = do
      FCMNotificationBody <$> getReallocationText
    getReallocationText =
      return $
        unwords
          [ "The driver had to cancel the ride for",
            showTimeIst (booking.startTime) <> ".",
            "Please wait until we allocate other driver."
          ]

notifyOnQuoteReceived ::
  ( HasCacheConfig r,
    CoreMetrics m,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
  DQuote.Quote ->
  m ()
notifyOnQuoteReceived quote = do
  searchRequest <- QSearchReq.findById quote.requestId >>= fromMaybeM (SearchRequestDoesNotExist quote.requestId.getId)
  person <- Person.findById searchRequest.riderId >>= fromMaybeM (PersonNotFound searchRequest.riderId.getId)
  config <- getFCMConfig person.merchantId
  let notificationData = mkNotificationData
  FCM.notifyPerson config notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken
  where
    mkNotificationData = do
      let title = FCMNotificationTitle $ T.pack "Quote received!"
          body =
            FCMNotificationBody $
              unwords
                [ "New quote received with price",
                  show quote.estimatedFare <> "."
                ]
      FCM.FCMData
        { fcmNotificationType = FCM.QUOTE_RECEIVED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = getId quote.requestId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.REALLOCATE_PRODUCT
        }

notifyDriverOnTheWay ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  Id Person ->
  m ()
notifyDriverOnTheWay personId = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  config <- getFCMConfig person.merchantId
  let notificationData =
        FCM.FCMData
          { fcmNotificationType = FCM.DRIVER_ON_THE_WAY,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = getId personId,
            fcmEntityData = (),
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.DRIVER_ON_THE_WAY
          }
      title = FCMNotificationTitle $ T.pack "Driver On The Way!"
      body =
        FCMNotificationBody $
          unwords
            [ "Driver is on the way"
            ]
  FCM.notifyPerson config notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken

notifyDriverHasReached ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  Id Person ->
  SRide.Ride ->
  m ()
notifyDriverHasReached personId ride = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  config <- getFCMConfig person.merchantId
  let notificationData =
        FCM.FCMData
          { fcmNotificationType = FCM.DRIVER_HAS_REACHED,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = getId personId,
            fcmEntityData = (),
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.DRIVER_HAS_REACHED
          }
      title = FCMNotificationTitle $ T.pack "Driver Has Reached!"
      body =
        FCMNotificationBody $
          unwords
            [ "Vehicle No. " <> ride.vehicleNumber <> " has reached your location. Use OTP " <> ride.otp <> " to verify the ride"
            ]
  FCM.notifyPerson config notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken
