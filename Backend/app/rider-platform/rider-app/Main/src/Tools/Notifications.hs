{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Notifications where

import qualified BecknV2.OnDemand.Enums as BecknEnums
import Data.Default.Class
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time hiding (secondsToNominalDiffTime)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.BppDetails as DBppDetails
import Domain.Types.EmptyDynamicParam
import Domain.Types.Estimate (Estimate)
import qualified Domain.Types.EstimateStatus as DEstimate
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Domain.Types.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
import qualified Domain.Types.NotificationSoundsConfig as NSC
import Domain.Types.Person as Person
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RefundRequest as DRefundRequest
import Domain.Types.RegistrationToken as RegToken
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Ride as SRide
import Domain.Types.RiderConfig as DRC
import Domain.Types.SearchRequest as SearchRequest
import Domain.Types.Trip (TripCategory)
import qualified Domain.Types.Trip as Trip
import qualified Domain.Types.VehicleVariant as VehicleVariant
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification as Notification
import qualified Kernel.External.Notification.FCM.Types as FCMType
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude (getField)
import qualified Kernel.Prelude as Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto hiding (count, runInReplica)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Price as Price
import Kernel.Utils.Common hiding (getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.JobScheduler
import qualified SharedLogic.MessageBuilder as MessageBuilder
import SharedLogic.Quote
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.FollowRide as CQFollowRide
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.Sos as CQSos
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.NotificationSoundsConfig as SQNSC
import qualified Storage.Queries.Person as Person
import Storage.Queries.PersonDefaultEmergencyNumber as QPDEN
import qualified Storage.Queries.PersonDisability as PD
import Storage.Queries.SafetySettings as QSafety
import qualified Storage.Queries.SearchRequest as QSearchReq
import Tools.Error
import qualified Tools.SMS as Sms
import qualified Tools.SharedRedisKeys as SharedRedisKeys
import qualified UrlShortner.Common as UrlShortner

templateText :: Text -> Text
templateText txt = "{#" <> txt <> "#}"

createNotificationReq :: Text -> (NotificationRequest -> NotificationRequest) -> NotificationRequest
createNotificationReq key modifier = modifier $ def {notificationKey = key}

data NotificationRequest = NotificationRequest
  { subCategory :: Maybe Notification.SubCategory,
    notificationKey :: Text,
    priority :: Maybe Notification.MessagePriority,
    ttl :: Maybe UTCTime,
    showType :: Notification.ShowNotification,
    soundTag :: Maybe Text,
    notificationTypeForSound :: Maybe Notification.Category,
    auth :: Maybe Notification.Auth
  }
  deriving (Show, Eq)

instance Default NotificationRequest where
  def =
    NotificationRequest
      { subCategory = Nothing,
        notificationKey = mempty,
        priority = Nothing,
        ttl = Nothing,
        showType = Notification.SHOW,
        soundTag = Nothing,
        notificationTypeForSound = Nothing,
        auth = Nothing
      }

buildTemplate :: [(Text, Text)] -> Text -> Text
buildTemplate paramVars template =
  foldl'
    ( \msg (findKey, replaceVal) ->
        T.replace (templateText findKey) replaceVal msg
    )
    template
    paramVars

buildTrackingUrl :: Id SRide.Ride -> [(Text, Text)] -> Text -> Text
buildTrackingUrl rideId extraQueryParams trackingUrlPattern = (buildTemplate extraQueryParams trackingUrlPattern) <> rideId.getId

notifyPerson ::
  ( ServiceFlow m r,
    ToJSON a,
    ToJSON b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Id Person ->
  Notification.NotificationReq a b ->
  Maybe FCMType.LiveActivityReq ->
  m ()
notifyPerson = runWithServiceConfig Notification.notifyPerson (.notifyPerson)

clearDeviceToken :: (MonadFlow m, EsqDBFlow m r) => Id Person -> m ()
clearDeviceToken = Person.clearDeviceTokenByPersonId

runWithServiceConfig ::
  ServiceFlow m r =>
  (Notification.NotificationServiceConfig -> req -> liveActivityReq -> m () -> m resp) ->
  (MerchantServiceUsageConfig -> Notification.NotificationService) ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Id Person ->
  req ->
  liveActivityReq ->
  m resp
runWithServiceConfig func getCfg merchantId merchantOperatingCityId personId req liveActivityReq = do
  merchantConfig <- QMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
  merchantNotificationServiceConfig <-
    QMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (DMSC.NotificationService $ getCfg merchantConfig)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "notification" (show $ getCfg merchantConfig))
  case merchantNotificationServiceConfig.serviceConfig of
    DMSC.NotificationServiceConfig msc -> func msc req liveActivityReq (clearDeviceToken personId)
    _ -> throwError $ InternalError "Unknown ServiceConfig"

-- dynamicNotifyPerson person notificationRequest notifyType dynamicParam entity tripCategory [varparams]
dynamicNotifyPerson ::
  (ServiceFlow m r, ToJSON a, ToJSON b) =>
  Person.Person ->
  NotificationRequest ->
  a ->
  Notification.Entity b ->
  Maybe TripCategory ->
  [(Text, Text)] ->
  Maybe [LYT.ConfigVersionMap] ->
  Maybe FCMType.LiveActivityReq ->
  m ()
dynamicNotifyPerson person notiData dynamicParams entity tripCategory dynamicTemplateParams mbConfigVersionMap liveActivityReq = do
  let merchantOperatingCityId = person.merchantOperatingCityId
  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOperatingCityId notiData.notificationKey tripCategory notiData.subCategory person.language mbConfigVersionMap
  when (EulerHS.Prelude.isNothing mbMerchantPN) $ logError $ "MISSED_FCM - " <> notiData.notificationKey
  whenJust mbMerchantPN \merchantPN -> do
    when (merchantPN.shouldTrigger) $ do
      let soundNotificationType = fromMaybe (merchantPN.fcmNotificationType) notiData.notificationTypeForSound
      notificationSoundFromConfig <- SQNSC.findByNotificationType soundNotificationType merchantOperatingCityId
      notificationSound <- getNotificationSound notiData.soundTag notificationSoundFromConfig
      let title = buildTemplate dynamicTemplateParams merchantPN.title
          body = buildTemplate dynamicTemplateParams merchantPN.body
          notificationData =
            Notification.NotificationReq
              { category = merchantPN.fcmNotificationType,
                subCategory = notiData.subCategory,
                showNotification = notiData.showType,
                messagePriority = notiData.priority,
                entity = entity,
                body = body,
                title = title,
                auth = fromMaybe (Notification.Auth person.id.getId person.deviceToken person.notificationToken) notiData.auth,
                sound = notificationSound,
                ttl = notiData.ttl,
                dynamicParams = dynamicParams
              }
      --logDebug $ "DFCM - " <> show notiData.notificationKey <> " Title -> " <> show title <> " body - " <> show body
      notifyPerson person.merchantId merchantOperatingCityId person.id notificationData liveActivityReq

--------------------------------------------------------------------------------------------------

notifyOnDriverOfferIncoming ::
  (ServiceFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  Id Estimate ->
  Maybe TripCategory ->
  [DQuote.Quote] ->
  Person.Person ->
  [DBppDetails.BppDetails] ->
  m ()
notifyOnDriverOfferIncoming estimateId tripCategory quotes person bppDetailList = do
  isValueAddNPList <- Prelude.for bppDetailList $ \bpp -> CQVAN.isValueAddNP bpp.subscriberId
  let entity = Notification.Entity Notification.Product estimateId.getId $ mkQAPIEntityList quotes bppDetailList isValueAddNPList
      notiReq = createNotificationReq "DRIVER_QUOTE_INCOMING" identity
  dynamicNotifyPerson person notiReq EmptyDynamicParam entity tripCategory mempty Nothing Nothing

-- title = "New driver offers incoming!"
-- body = "There are new driver offers! Check the app for details"

notifyOnRideSearchExpired ::
  (ServiceFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  SearchRequest ->
  m ()
notifyOnRideSearchExpired searchReq = do
  logDebug "Sending ride search expired notification"
  let searchRequestId = searchReq.id
  person <- Person.findById searchReq.riderId
  mbJourneyLeg <- QJourneyLeg.findByLegSearchId (Just searchReq.id.getId)
  whenJust (mbJourneyLeg >>= (.legPricingId)) $ QEstimate.updateStatus DEstimate.RIDE_SEARCH_EXPIRED . Id
  case person of
    Just personObj -> do
      let entity = Notification.Entity Notification.SearchRequest searchRequestId.getId ()
          dynamicParams = EmptyDynamicParam
          destination = do
            loc <- getField @"toLocation" searchReq
            let addr = getField @"address" loc
            getField @"building" addr
              <|> getField @"title" addr
              <|> getField @"area" addr
              <|> getField @"street" addr
              <|> getField @"city" addr
          liveActivityCustomMessage = Just "Your ride search has expired!!"
      dynamicNotifyPerson
        personObj
        (createNotificationReq "EXPIRED_CASE" identity)
        dynamicParams
        entity
        Nothing
        []
        (Just searchReq.configInExperimentVersions)
        ( case personObj.liveActivityToken of
            Just _liveActivityToken ->
              Just $
                FCMType.LiveActivityReq
                  { liveActivityToken = _liveActivityToken,
                    liveActivityReqType = "end",
                    liveActivityNotificationType = "CANCELLED_PRODUCT",
                    liveActivityContentState =
                      FCMType.LiveActivityContentState
                        { activityStatus = "CANCELLED",
                          driverInfo = Nothing,
                          bookingInfo =
                            Just $
                              FCMType.BookingInfo
                                { vehicleColor = Nothing,
                                  vehicleName = Nothing,
                                  vehicleNumber = Nothing,
                                  vehicleVariant = Nothing,
                                  source = Nothing,
                                  destination = destination,
                                  estimatedFare = Nothing
                                },
                          timerDuration = Nothing,
                          customMessage = liveActivityCustomMessage
                        },
                    liveActivityApnsPriority = "10"
                  }
            _ -> Nothing
        )
    _ -> pure ()

-- title = "Ride search expired!"
-- body = "Your ride search has expired. Please retry"

data RideAssignedParam = RideAssignedParam
  { driverName :: Text,
    rideTime :: UTCTime,
    bookingId :: Id SRB.Booking,
    isScheduledBooking :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyOnRideAssigned ::
  ServiceFlow m r =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnRideAssigned booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow person.merchantOperatingCityId booking.configInExperimentVersions >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)

  -- Check if vehicle number matches any special vehicle notification config
  let matchedSpecialVehicleConfig = case riderConfig.specialVehicleNotificationConfigs of
        Just configs -> L.find (\config -> config.vehicleNo == ride.vehicleNumber) configs
        Nothing -> Nothing

  let entity = Notification.Entity Notification.Product rideId.getId (RideAssignedParam driverName booking.startTime booking.id booking.isScheduled)
      dynamicParams = RideAssignedParam driverName booking.startTime booking.id booking.isScheduled
  allOtherBookingPartyPersons <- getAllOtherRelatedPartyPersons booking

  forM_ (person : allOtherBookingPartyPersons) $ \person' -> do
    tag <- getDisabilityTag person.hasDisability person'.id

    -- If vehicle number matches, send custom notification first
    whenJust matchedSpecialVehicleConfig $ \specialConfig -> do
      notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.DRIVER_ASSIGNMENT person'.merchantOperatingCityId
      notificationSound <- getNotificationSound tag notificationSoundFromConfig
      let customNotificationData =
            Notification.NotificationReq
              { category = Notification.DRIVER_ASSIGNMENT,
                subCategory = Nothing,
                showNotification = Notification.SHOW,
                messagePriority = Nothing,
                entity = entity,
                body = specialConfig.notificationMessage,
                title = specialConfig.notificationTitle,
                auth = Notification.Auth person'.id.getId person'.deviceToken person'.notificationToken,
                sound = notificationSound,
                ttl = Nothing,
                dynamicParams = dynamicParams
              }
      notifyPerson person'.merchantId person'.merchantOperatingCityId person'.id customNotificationData Nothing

    -- Always send the normal DRIVER_ASSIGNMENT notification
    dynamicNotifyPerson
      person'
      (createNotificationReq "DRIVER_ASSIGNMENT" (\r -> r {soundTag = tag}))
      dynamicParams
      entity
      booking.tripCategory
      [("driverName", driverName)]
      (Just booking.configInExperimentVersions)
      ( case person.liveActivityToken of
          Just _liveActivityToken ->
            Just $
              FCMType.LiveActivityReq
                { liveActivityToken = _liveActivityToken,
                  liveActivityReqType = "update",
                  liveActivityNotificationType = "DRIVER_ASSIGNMENT",
                  liveActivityContentState =
                    FCMType.LiveActivityContentState
                      { activityStatus = "ASSIGNED",
                        driverInfo =
                          Just $
                            FCMType.DriverInfo
                              { rideOtp = Just ride.otp,
                                driverName = Nothing,
                                distanceLeft = Nothing,
                                totalDistance = Nothing,
                                driverNumber = Nothing,
                                driverProfile = Nothing
                              },
                        bookingInfo =
                          Just $
                            FCMType.BookingInfo
                              { vehicleColor = ride.vehicleColor,
                                vehicleName = Just ride.vehicleModel,
                                vehicleNumber = Just ride.vehicleNumber,
                                vehicleVariant = Just (show ride.vehicleVariant),
                                source = Nothing,
                                destination = Nothing,
                                estimatedFare = Nothing
                              },
                        timerDuration = Nothing,
                        customMessage = Nothing
                      },
                  liveActivityApnsPriority = "10"
                }
          _ -> Nothing
      )

notifyOnScheduledRideAccepted ::
  ServiceFlow m r =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnScheduledRideAccepted booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.DRIVER_ASSIGNMENT merchantOperatingCityId
  tag <- getDisabilityTag person.hasDisability personId
  notificationSound <- getNotificationSound tag notificationSoundFromConfig
  let notificationData =
        Notification.NotificationReq
          { category = Notification.DRIVER_ASSIGNMENT,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product rideId.getId (RideAssignedParam driverName booking.startTime booking.id booking.isScheduled),
            body = body,
            title = title,
            dynamicParams = RideAssignedParam driverName booking.startTime booking.id booking.isScheduled,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
      title = T.pack "Driver assigned!"
      body =
        unwords
          [ driverName,
            "will be your driver for your scheduled trip starting at ",
            showTimeIst booking.startTime
          ]
  notifyPerson person.merchantId merchantOperatingCityId person.id notificationData Nothing

newtype RideStartedParam = RideStartedParam
  { driverName :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype TripAssignedData = TripAssignedData
  { tripCategory :: Maybe TripCategory
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyOnRideStarted ::
  ServiceFlow m r =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnRideStarted booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
      serviceTierName = fromMaybe (show booking.vehicleServiceTierType) booking.serviceTierName
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let entity = Notification.Entity Notification.Product rideId.getId (TripAssignedData booking.tripCategory)
      dynamicParams = RideStartedParam driverName
      toLocationDestination = do
        loc <- ride.toLocation
        let addr = loc.address
        addr.building <|> addr.title <|> addr.area <|> addr.street <|> addr.city
  -- finding other booking parties for delivery --
  allOtherBookingPartyPersons <- getAllOtherRelatedPartyPersons booking
  forM_ (person : allOtherBookingPartyPersons) $ \person' -> do
    tag <- getDisabilityTag person.hasDisability person'.id
    dynamicNotifyPerson
      person'
      (createNotificationReq "TRIP_STARTED" (\r -> r {soundTag = tag}))
      dynamicParams
      entity
      booking.tripCategory
      [ ("driverName", driverName),
        ("serviceTierName", serviceTierName)
      ]
      (Just booking.configInExperimentVersions)
      ( case person.liveActivityToken of
          Just _liveActivityToken ->
            Just $
              FCMType.LiveActivityReq
                { liveActivityToken = _liveActivityToken,
                  liveActivityReqType = "update",
                  liveActivityNotificationType = "TRIP_STARTED",
                  liveActivityContentState =
                    FCMType.LiveActivityContentState
                      { activityStatus = "ONRIDE",
                        driverInfo = Nothing,
                        bookingInfo =
                          Just $
                            FCMType.BookingInfo
                              { vehicleColor = ride.vehicleColor,
                                vehicleName = Just ride.vehicleModel,
                                vehicleNumber = Just ride.vehicleNumber,
                                vehicleVariant = Just (show ride.vehicleVariant),
                                source = Nothing,
                                destination = toLocationDestination,
                                estimatedFare = Just (show booking.estimatedFare)
                              },
                        timerDuration = Nothing,
                        customMessage = Nothing
                      },
                  liveActivityApnsPriority = "10"
                }
          _ -> Nothing
      )

-- title = "Your {#serviceTierName#} ride has started!"
-- body = "Your {#serviceTierName#} ride with {#driverName#} has started. Enjoy the ride!"

data RideCompleteParam = RideCompleteParam
  { driverName :: Text,
    fare :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyOnRideCompleted ::
  (ServiceFlow m r, SchedulerFlow r, HasField "blackListedJobs" r [Text]) =>
  SRB.Booking ->
  SRide.Ride ->
  [Person.Person] ->
  m ()
notifyOnRideCompleted booking ride otherParties = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
      mbTotalFare = ride.totalFare
      totalFare = fromMaybe booking.estimatedFare mbTotalFare
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let entity = Notification.Entity Notification.Product rideId.getId ()
      dynamicParams = RideCompleteParam driverName $ show totalFare.amountInt
      toLocationDestination = do
        loc <- ride.toLocation
        let addr = loc.address
        addr.building <|> addr.title <|> addr.area <|> addr.street <|> addr.city
  disableFollowRide personId
  Redis.del $ CQSos.mockSosKey personId
  forM_ (person : otherParties) $ \person' -> do
    tag <- getDisabilityTag person.hasDisability person'.id
    dynamicNotifyPerson
      person'
      (createNotificationReq "TRIP_FINISHED" (\r -> r {soundTag = tag}))
      dynamicParams
      entity
      booking.tripCategory
      [ ("driverName", driverName),
        ("totalFare", showPriceWithRounding totalFare)
      ]
      (Just booking.configInExperimentVersions)
      ( case person.liveActivityToken of
          Just _liveActivityToken ->
            Just $
              FCMType.LiveActivityReq
                { liveActivityToken = _liveActivityToken,
                  liveActivityReqType = "end",
                  liveActivityNotificationType = "TRIP_FINISHED",
                  liveActivityContentState =
                    FCMType.LiveActivityContentState
                      { activityStatus = "COMPLETED",
                        driverInfo = Nothing,
                        bookingInfo =
                          Just $
                            FCMType.BookingInfo
                              { vehicleColor = ride.vehicleColor,
                                vehicleName = Just ride.vehicleModel,
                                vehicleNumber = Just ride.vehicleNumber,
                                vehicleVariant = Just (show ride.vehicleVariant),
                                source = Nothing,
                                destination = toLocationDestination,
                                estimatedFare = Just (showPriceWithRounding totalFare)
                              },
                        timerDuration = Nothing,
                        customMessage = Nothing
                      },
                  liveActivityApnsPriority = "10"
                }
          _ -> Nothing
      )

  fork "Create Post ride safety job" $ do
    safetySettings <- QSafety.findSafetySettingsWithFallback person.id (Just person)
    riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow person.merchantOperatingCityId booking.configInExperimentVersions >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
    now <- getLocalCurrentTime riderConfig.timeDiffFromUtc
    when (checkSafetySettingConstraint (Just safetySettings.enablePostRideSafetyCheck) riderConfig now) $ do
      let scheduleAfter = riderConfig.postRideSafetyNotificationDelay
          postRideSafetyNotificationJobData = PostRideSafetyNotificationJobData {rideId = ride.id, personId = booking.riderId}
      createJobIn @_ @'PostRideSafetyNotification ride.merchantId ride.merchantOperatingCityId scheduleAfter (postRideSafetyNotificationJobData :: PostRideSafetyNotificationJobData)

-- title = "Trip finished!"
-- body = "Hope you enjoyed your trip with {#driverName#}. Total Fare {#totalFare#}"

disableFollowRide ::
  ServiceFlow m r =>
  Id Person ->
  m ()
disableFollowRide personId = do
  emContacts <- QPDEN.findAllByPersonId personId
  let followingContacts = filter (\item -> item.shareTripWithEmergencyContactOption /= Just NEVER_SHARE || item.enableForShareRide) emContacts
  mapM_
    ( \contact -> maybe (pure ()) updateFollowRideCount contact.contactPersonId
    )
    followingContacts
  void $ QPDEN.updateShareRideForAll personId False
  where
    updateFollowRideCount emPersonId = do
      CQFollowRide.updateFollowRideList emPersonId personId False
      list <- CQFollowRide.getFollowRideCounter emPersonId
      when (L.null list) $ do
        CQFollowRide.clearFollowsRideCounter emPersonId
        Person.updateFollowsRide False emPersonId

notifyOnExpiration ::
  ServiceFlow m r =>
  SearchRequest ->
  m ()
notifyOnExpiration searchReq = do
  let searchRequestId = searchReq.id
  let personId = searchReq.riderId
  person <- Person.findById personId
  let tripCategory = case searchReq.riderPreferredOption of
        SearchRequest.Delivery -> Just (Trip.Delivery Trip.OneWayOnDemandDynamicOffer)
        _ -> Nothing
  case person of
    Just p -> do
      let entity = Notification.Entity Notification.SearchRequest searchRequestId.getId ()
          dynamicParams = EmptyDynamicParam
      dynamicNotifyPerson
        p
        (createNotificationReq "EXPIRED_CASE" identity)
        dynamicParams
        entity
        tripCategory
        []
        (Just searchReq.configInExperimentVersions)
        Nothing
    _ -> pure ()

-- title = T.pack "Ride expired!"
-- body =
--   unwords
--     [ "Your ride has expired as you did not confirm any offer.",
--       "Please book again to continue."
--     ]

notifyOnRegistration ::
  ServiceFlow m r =>
  RegistrationToken ->
  Person ->
  Maybe Text ->
  m ()
notifyOnRegistration regToken person mbDeviceToken = do
  let tokenId = RegToken.id regToken
      entity = Notification.Entity Notification.Merchant tokenId.getId ()
      auth = Just $ Notification.Auth person.id.getId mbDeviceToken person.notificationToken
  dynamicNotifyPerson
    person
    (createNotificationReq "REGISTRATION_APPROVED" (\r -> r {auth = auth}))
    EmptyDynamicParam
    entity
    Nothing
    []
    Nothing
    Nothing

-- title = T.pack "Registration Completed!"
-- body =
--   unwords
--     [ "Welcome to Yatri.",
--       "Click here to book your first ride with us."
--     ]

data RideCancelParam = RideCancelParam
  { rideTime :: UTCTime,
    bookingId :: Id SRB.Booking
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyOnBookingCancelled ::
  ServiceFlow m r =>
  SRB.Booking ->
  SBCR.CancellationSource ->
  DBppDetails.BppDetails ->
  Maybe DRide.Ride ->
  [Person.Person] ->
  m ()
notifyOnBookingCancelled booking cancellationSource bppDetails mbRide otherParties = do
  person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  let notificationSoundType = case cancellationSource of
        SBCR.ByDriver -> do
          case mbRide of
            Nothing -> Notification.CANCELLED_PRODUCT
            Just _ -> Notification.CANCELLED_PRODUCT_DRIVER
        SBCR.ByUser -> Notification.CANCELLED_PRODUCT_USER
        _ -> Notification.CANCELLED_PRODUCT
      liveActivityCustomMessage = case cancellationSource of
        SBCR.ByDriver -> do
          case mbRide of
            Nothing -> Just "Sorry, No drivers found!"
            Just _ -> Just "The driver had to cancel the ride!"
        SBCR.ByUser -> Just "You have cancelled your ride!"
        _ -> Just "Sorry, No drivers found!"
  fork "Disabling share ride" $ do
    disableFollowRide person.id
    Redis.del $ CQSos.mockSosKey person.id
  let notiKey = case mbRide of
        Just _ -> "BOOKING_CANCEL_WITH_RIDE"
        Nothing -> "BOOKING_CANCEL_WITH_NO_RIDE"
      entity = Notification.Entity Notification.Product booking.id.getId (RideCancelParam booking.startTime booking.id)
      dynamicParams = RideCancelParam booking.startTime booking.id
      toLocationDestination = do
        destinationAdd <- case booking.bookingDetails of
          SRB.RentalDetails _ -> Nothing
          SRB.OneWayDetails details -> Just details.toLocation
          SRB.DriverOfferDetails details -> Just details.toLocation
          SRB.OneWaySpecialZoneDetails details -> Just details.toLocation
          SRB.InterCityDetails details -> Just details.toLocation
          SRB.AmbulanceDetails details -> Just details.toLocation
          SRB.DeliveryDetails details -> Just details.toLocation
          SRB.MeterRideDetails details -> details.toLocation
        let addr = destinationAdd.address
        addr.building <|> addr.title <|> addr.area <|> addr.street <|> addr.city
  forM_ (person : otherParties) $ \person' -> do
    tag <- getDisabilityTag person.hasDisability person'.id
    dynamicNotifyPerson
      person'
      ( createNotificationReq
          notiKey
          ( \r ->
              r
                { notificationTypeForSound = Just notificationSoundType,
                  subCategory = Just $ cancellationSourceToSubCategory cancellationSource,
                  soundTag = tag
                }
          )
      )
      dynamicParams
      entity
      booking.tripCategory
      [ ("bookingStartTime", showTimeIst (booking.startTime)),
        ("orgName", bppDetails.name)
      ]
      (Just booking.configInExperimentVersions)
      ( case person.liveActivityToken of
          Just _liveActivityToken ->
            Just $
              FCMType.LiveActivityReq
                { liveActivityToken = _liveActivityToken,
                  liveActivityReqType = "end",
                  liveActivityNotificationType = "CANCELLED_PRODUCT",
                  liveActivityContentState =
                    FCMType.LiveActivityContentState
                      { activityStatus = "CANCELLED",
                        driverInfo = Nothing,
                        bookingInfo =
                          Just $
                            FCMType.BookingInfo
                              { vehicleColor = Nothing,
                                vehicleName = Nothing,
                                vehicleNumber = Nothing,
                                vehicleVariant = Nothing,
                                source = Nothing,
                                destination = toLocationDestination,
                                estimatedFare = Nothing
                              },
                        timerDuration = Nothing,
                        customMessage = liveActivityCustomMessage
                      },
                  liveActivityApnsPriority = "10"
                }
          _ -> Nothing
      )

cancellationSourceToSubCategory :: SBCR.CancellationSource -> Notification.SubCategory
cancellationSourceToSubCategory = \case
  SBCR.ByUser -> Notification.ByUser
  SBCR.ByMerchant -> Notification.ByMerchant
  SBCR.ByDriver -> Notification.ByDriver
  SBCR.ByAllocator -> Notification.ByAllocator
  SBCR.ByApplication -> Notification.ByApplication

-- Notification.CANCELLED_PRODUCT
-- BOOKING_CANCEL_WITH_RIDE
-- BOOKING_CANCEL_WITH_NO_RIDE
-- getTitle = case mbRide of
--   Just _ -> T.pack "Ride cancelled!"
--   Nothing -> case cancellationSource of
--     SBCR.ByUser -> T.pack "Ride cancelled!"
--     _ -> T.pack "Ride Unavailable!"
-- getCancellationText orgName = case cancellationSource of
--   SBCR.ByUser ->
--     unwords
--       [ "You have cancelled your ride for",
--         showTimeIst (booking.startTime) <> ".",
--         "Check the app for details."
--       ]
--   SBCR.ByMerchant ->
--     unwords
--       [ "\"" <> orgName <> "\" agency had to cancel the ride for",
--         showTimeIst (booking.startTime) <> ".",
--         "Please book again to get another ride."
--       ]
--   SBCR.ByDriver ->
--     case mbRide of
--       Nothing ->
--         unwords
--           [ "Sorry, we could not find any driver for your ride at",
--             showTimeIst (booking.startTime) <> ".",
--             "Please try to book again"
--           ]
--       Just _ ->
--         unwords
--           [ "The driver had to cancel the ride for",
--             showTimeIst (booking.startTime) <> ".",
--             "Please book again to get another ride."
--           ]
--   SBCR.ByAllocator ->
--     unwords
--       [ "The ride for",
--         showTimeIst (booking.startTime),
--         "was cancelled as we could not find a driver.",
--         "Please book again to get another ride."
--       ]
--   SBCR.ByApplication ->
--     case mbRide of
--       Nothing ->
--         unwords
--           [ "Sorry, we could not find any driver for your ride at",
--             showTimeIst (booking.startTime) <> ".",
--             "Please try to book again"
--           ]
--       Just _ ->
--         unwords
--           [ "Sorry your ride for",
--             showTimeIst (booking.startTime),
--             "was cancelled.",
--             "Please try to book again"
--           ]

data BookingReallocatedParam = BookingReallocatedParam
  { rideTime :: UTCTime,
    bookingId :: Id SRB.Booking
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyOnBookingReallocated ::
  ServiceFlow m r =>
  SRB.Booking ->
  m ()
notifyOnBookingReallocated booking = do
  person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  let entity = Notification.Entity Notification.Product booking.id.getId ()
      dynamicParams = BookingReallocatedParam booking.startTime booking.id
  allOtherBookingPartyPersons <- getAllOtherRelatedPartyPersons booking
  forM_ (person : allOtherBookingPartyPersons) $ \person' -> do
    tag <- getDisabilityTag person.hasDisability person'.id
    dynamicNotifyPerson
      person'
      (createNotificationReq "BOOKING_REALLOCATED" (\r -> r {soundTag = tag}))
      dynamicParams
      entity
      booking.tripCategory
      [("bookingStartTime", showTimeIst (booking.startTime))]
      (Just booking.configInExperimentVersions)
      Nothing

-- title = T.pack "Ride cancelled! We are allocating another driver"
-- body =
--   unwords
--     [ "The driver had to cancel the ride for",
--       showTimeIst (booking.startTime) <> ".",
--       "Please wait until we allocate another driver."
--     ]

notifyOnEstOrQuoteReallocated ::
  (ServiceFlow m r, CacheFlow m r) =>
  SBCR.CancellationSource ->
  SRB.Booking ->
  Text ->
  m ()
notifyOnEstOrQuoteReallocated cancellationSource booking estOrQuoteId = do
  person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  tag <- getDisabilityTag person.hasDisability person.id
  batchConfig <- SharedRedisKeys.getBatchConfig booking.transactionId
  let entity = Notification.Entity Notification.Product estOrQuoteId (BookingReallocatedParam booking.startTime booking.id)
      subCategory = cancellationSourceToSubCategory cancellationSource
      toLocationDestination = do
        destinationAdd <- case booking.bookingDetails of
          SRB.RentalDetails _ -> Nothing
          SRB.OneWayDetails details -> Just details.toLocation
          SRB.DriverOfferDetails details -> Just details.toLocation
          SRB.OneWaySpecialZoneDetails details -> Just details.toLocation
          SRB.InterCityDetails details -> Just details.toLocation
          SRB.AmbulanceDetails details -> Just details.toLocation
          SRB.DeliveryDetails details -> Just details.toLocation
          SRB.MeterRideDetails details -> details.toLocation
        let addr = destinationAdd.address
        addr.building <|> addr.title <|> addr.area <|> addr.street <|> addr.city
  dynamicNotifyPerson
    person
    (createNotificationReq "EST_OR_QUOTE_REALLOCATED" (\r -> r {soundTag = tag, subCategory = Just subCategory}))
    EmptyDynamicParam
    entity
    booking.tripCategory
    [("bookingStartTime", showTimeIst (booking.startTime))]
    (Just booking.configInExperimentVersions)
    ( case person.liveActivityToken of
        Just _liveActivityToken ->
          Just $
            FCMType.LiveActivityReq
              { liveActivityToken = _liveActivityToken,
                liveActivityReqType = "update",
                liveActivityNotificationType = "EST_OR_QUOTE_REALLOCATED",
                liveActivityContentState =
                  FCMType.LiveActivityContentState
                    { activityStatus = "REALLOCATION",
                      driverInfo = Nothing,
                      bookingInfo =
                        Just $
                          FCMType.BookingInfo
                            { vehicleColor = Nothing,
                              vehicleName = Nothing,
                              vehicleNumber = Nothing,
                              vehicleVariant = Nothing,
                              source = Nothing,
                              destination = toLocationDestination,
                              estimatedFare = Nothing
                            },
                      timerDuration = batchConfig,
                      customMessage = Nothing
                    },
                liveActivityApnsPriority = "10"
              }
        _ -> Nothing
    )

-- title = T.pack "Searching for a New Driver!"
-- body = case cancellationSource of
--   SBCR.ByUser ->
--     unwords
--       [ "You have cancelled your ride for",
--         showTimeIst (booking.startTime) <> ".",
--         "Please wait while we allocate you another driver."
--       ]
--   SBCR.ByMerchant ->
--     unwords
--       [ "The ride for",
--         showTimeIst (booking.startTime),
--         "is cancelled. Please wait while we allocate you another driver."
--       ]
--   SBCR.ByDriver ->
--     unwords
--       [ "The driver had cancelled the ride for",
--         showTimeIst (booking.startTime) <> ".",
--         "Please wait while we allocate you another driver."
--       ]
--   SBCR.ByAllocator ->
--     unwords
--       [ "The ride for",
--         showTimeIst (booking.startTime),
--         "is cancelled. Please wait while we allocate you another driver."
--       ]
--   SBCR.ByApplication ->
--     unwords
--       [ "Sorry your ride for",
--         showTimeIst (booking.startTime),
--         "was cancelled.",
--         "Please wait while we allocate you another driver."
--       ]

notifyOnQuoteReceived ::
  ServiceFlow m r =>
  DQuote.Quote ->
  m ()
notifyOnQuoteReceived quote = do
  searchRequest <- QSearchReq.findById quote.requestId >>= fromMaybeM (SearchRequestDoesNotExist quote.requestId.getId)
  person <- Person.findById searchRequest.riderId >>= fromMaybeM (PersonNotFound searchRequest.riderId.getId)
  let entity = Notification.Entity Notification.Product quote.requestId.getId ()
      dynamicParams = EmptyDynamicParam
      tripCategory = case searchRequest.riderPreferredOption of
        SearchRequest.Delivery -> Just (Trip.Delivery Trip.OneWayOnDemandDynamicOffer)
        _ -> Nothing
  dynamicNotifyPerson
    person
    (createNotificationReq "QUOTE_RECEIVED" identity)
    dynamicParams
    entity
    tripCategory
    [("quoteFareEstimate", show quote.estimatedFare)]
    (Just searchRequest.configInExperimentVersions)
    Nothing

-- title = T.pack "Quote received!"
-- body =
--   unwords
--     [ "New quote received with price",
--       show quote.estimatedFare <> "."
--     ]

notifyDriverOnTheWay ::
  ServiceFlow m r =>
  Id Person ->
  Maybe TripCategory ->
  SRide.Ride ->
  m ()
notifyDriverOnTheWay personId tripCategory ride = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let entity = Notification.Entity Notification.Product personId.getId ()
  dynamicNotifyPerson
    person
    (createNotificationReq "DRIVER_ON_THE_WAY" identity)
    EmptyDynamicParam
    entity
    tripCategory
    []
    Nothing
    ( case person.liveActivityToken of
        Just _liveActivityToken ->
          Just $
            FCMType.LiveActivityReq
              { liveActivityToken = _liveActivityToken,
                liveActivityReqType = "update",
                liveActivityNotificationType = "DRIVER_ON_THE_WAY",
                liveActivityContentState =
                  FCMType.LiveActivityContentState
                    { activityStatus = "ONWAY",
                      driverInfo =
                        Just $
                          FCMType.DriverInfo
                            { rideOtp = Just ride.otp,
                              driverName = Nothing,
                              distanceLeft = Nothing,
                              totalDistance = Nothing,
                              driverNumber = Nothing,
                              driverProfile = Nothing
                            },
                      bookingInfo =
                        Just $
                          FCMType.BookingInfo
                            { vehicleColor = ride.vehicleColor,
                              vehicleName = Just ride.vehicleModel,
                              vehicleNumber = Just ride.vehicleNumber,
                              vehicleVariant = Just (show ride.vehicleVariant),
                              source = Nothing,
                              destination = Nothing,
                              estimatedFare = Nothing
                            },
                      timerDuration = Nothing,
                      customMessage = Nothing
                    },
                liveActivityApnsPriority = "10"
              }
        _ -> Nothing
    )

-- title = T.pack "Driver On The Way!"
-- body =
--   unwords
--     [ "Driver is on the way"
--     ]

data DriverReachedParam = DriverReachedParam
  { vehicleNumber :: Text,
    rideOtp :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyDriverHasReached ::
  ServiceFlow m r =>
  Id Person ->
  Maybe TripCategory ->
  Text ->
  Text ->
  Maybe Text ->
  Text ->
  VehicleVariant.VehicleVariant ->
  m ()
notifyDriverHasReached personId tripCategory otp vehicleNumber mbVehicleColor vehicleModel vehicleVariant = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let entity = Notification.Entity Notification.Product personId.getId ()
      dynamicParams = DriverReachedParam vehicleNumber otp
  dynamicNotifyPerson
    person
    (createNotificationReq "DRIVER_HAS_REACHED" identity)
    dynamicParams
    entity
    tripCategory
    [ ("otp", otp),
      ("vehicleNumber", vehicleNumber)
    ]
    Nothing
    ( case person.liveActivityToken of
        Just _liveActivityToken ->
          Just $
            FCMType.LiveActivityReq
              { liveActivityToken = _liveActivityToken,
                liveActivityReqType = "update",
                liveActivityNotificationType = "DRIVER_HAS_REACHED",
                liveActivityContentState =
                  FCMType.LiveActivityContentState
                    { activityStatus = "WAITING",
                      driverInfo =
                        Just $
                          FCMType.DriverInfo
                            { rideOtp = Just otp,
                              driverName = Nothing,
                              distanceLeft = Nothing,
                              totalDistance = Nothing,
                              driverNumber = Nothing,
                              driverProfile = Nothing
                            },
                      bookingInfo =
                        Just $
                          FCMType.BookingInfo
                            { vehicleColor = mbVehicleColor,
                              vehicleName = Just vehicleModel,
                              vehicleNumber = Just vehicleNumber,
                              vehicleVariant = Just (show vehicleVariant),
                              source = Nothing,
                              destination = Nothing,
                              estimatedFare = Nothing
                            },
                      timerDuration = Nothing,
                      customMessage = Nothing
                    },
                liveActivityApnsPriority = "10"
              }
        _ -> Nothing
    )

-- title = T.pack "Driver Has Reached!"
-- body =
--   unwords
--     [ "Use OTP " <> otp <> " to verify the ride with Vehicle No. " <> vehicleNumber
--     ]

notifyDriverReaching ::
  ServiceFlow m r =>
  Id Person ->
  Maybe TripCategory ->
  Text ->
  Text ->
  SRide.Ride ->
  m ()
notifyDriverReaching personId tripCategory otp vehicleNumber ride = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let entity = Notification.Entity Notification.Product personId.getId ()
      dynamicParams = DriverReachedParam vehicleNumber otp
  dynamicNotifyPerson
    person
    (createNotificationReq "DRIVER_REACHING" identity)
    dynamicParams
    entity
    tripCategory
    []
    Nothing
    ( case person.liveActivityToken of
        Just _liveActivityToken ->
          Just $
            FCMType.LiveActivityReq
              { liveActivityToken = _liveActivityToken,
                liveActivityReqType = "update",
                liveActivityNotificationType = "DRIVER_ARRIVING",
                liveActivityContentState =
                  FCMType.LiveActivityContentState
                    { activityStatus = "ARRIVING",
                      driverInfo =
                        Just $
                          FCMType.DriverInfo
                            { rideOtp = Just ride.otp,
                              driverName = Nothing,
                              distanceLeft = Nothing,
                              totalDistance = Nothing,
                              driverNumber = Nothing,
                              driverProfile = Nothing
                            },
                      bookingInfo =
                        Just $
                          FCMType.BookingInfo
                            { vehicleColor = ride.vehicleColor,
                              vehicleName = Just ride.vehicleModel,
                              vehicleNumber = Just ride.vehicleNumber,
                              vehicleVariant = Just (show ride.vehicleVariant),
                              source = Nothing,
                              destination = Nothing,
                              estimatedFare = Nothing
                            },
                      timerDuration = Nothing,
                      customMessage = Nothing
                    },
                liveActivityApnsPriority = "10"
              }
        _ -> Nothing
    )

-- title = T.pack "Driver Arriving Now!"
-- body =
--   unwords
--     [ "Your driver is arriving now! Please be at the pickup location"
--     ]

notifyOnNewMessage ::
  ( ServiceFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  SRB.Booking ->
  T.Text ->
  m ()
notifyOnNewMessage booking message = do
  person <- runInReplica $ Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.CHAT_MESSAGE merchantOperatingCityId
  let notificationSound = maybe Nothing NSC.defaultSound notificationSoundFromConfig
  let notificationData =
        Notification.NotificationReq
          { category = Notification.CHAT_MESSAGE,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product person.id.getId (),
            body = body,
            title = title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
      title = T.pack "Driver"
      body =
        unwords
          [ message
          ]
  notifyPerson person.merchantId merchantOperatingCityId person.id notificationData Nothing

notifySafetyAlert ::
  ServiceFlow m r =>
  SRB.Booking ->
  T.Text ->
  m ()
notifySafetyAlert booking code = do
  logDebug "Sending safety alert notification"
  person <- runInReplica $ Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  let entity = Notification.Entity Notification.Product person.id.getId ()
      key = mkSafetyNotificationKey code
  dynamicNotifyPerson
    person
    (createNotificationReq key identity)
    EmptyDynamicParam
    entity
    booking.tripCategory
    []
    (Just booking.configInExperimentVersions)
    Nothing

-- title = "Everything okay?"
-- body = "We noticed your ride is on a different route. Are you feeling safe on your trip?"

notifyDriverBirthDay ::
  ServiceFlow m r =>
  Id Person ->
  Maybe TripCategory ->
  Text ->
  m ()
notifyDriverBirthDay personId tripCategory driverName = do
  person <- runInReplica $ Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let entity = Notification.Entity Notification.Product personId.getId ()
  dynamicNotifyPerson
    person
    (createNotificationReq "DRIVER_BIRTHDAY" identity)
    EmptyDynamicParam
    entity
    tripCategory
    [("driverName", driverName)]
    Nothing
    Nothing

-- title = T.pack "Driver's Birthday!"
-- body =
--   unwords
--     [ "Today is your driver " <> driverName <> "'s birthday, your warm wishes will make their day even more special!"
--     ]

notifyRideStartToEmergencyContacts ::
  ( EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    ServiceFlow m r,
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig]
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyRideStartToEmergencyContacts booking ride = do
  rider <- runInReplica $ Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow rider.merchantOperatingCityId booking.configInExperimentVersions >>= fromMaybeM (RiderConfigDoesNotExist rider.merchantOperatingCityId.getId)
  now <- getLocalCurrentTime riderConfig.timeDiffFromUtc
  personENList <- QPDEN.findpersonENListWithFallBack booking.riderId (Just rider)
  let followingContacts = filter (\contact -> checkSafetySettingConstraint contact.shareTripWithEmergencyContactOption riderConfig now) personENList
  let shouldShare = not $ null followingContacts
  if shouldShare
    then do
      let trackLink = buildTrackingUrl ride.id [("vp", "shareRide")] riderConfig.trackingShortUrlPattern
      decEmContacts <- decrypt `mapM` followingContacts
      for_ decEmContacts \contact -> do
        case contact.contactPersonId of
          Just personId -> do
            updateFollowsRideCount personId
            person <- runInReplica $ Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
            let entity = Notification.Entity Notification.Product personId.getId ()
            dynamicNotifyPerson
              person
              (createNotificationReq "FOLLOW_RIDE" identity)
              EmptyDynamicParam
              entity
              booking.tripCategory
              [("name", fromMaybe "" rider.firstName)]
              (Just booking.configInExperimentVersions)
              Nothing
          -- title = T.pack "Follow Ride"
          -- body =
          --   unwords
          --     [ fromMaybe "" name,
          --       " wants you to follow their ride"
          --     ]
          Nothing -> sendSMS contact rider.firstName trackLink
    else logInfo "Follow ride is not enabled"
  where
    updateFollowsRideCount emPersonId = do
      void $ CQFollowRide.updateFollowRideList emPersonId booking.riderId True
      Person.updateFollowsRide True emPersonId

    sendSMS emergencyContact name trackLink = do
      shortenedTrackingUrl <- MessageBuilder.shortenTrackingUrl trackLink
      buildSmsReq <-
        MessageBuilder.buildFollowRideStartedMessage booking.merchantOperatingCityId $
          MessageBuilder.BuildFollowRideMessageReq
            { userName = fromMaybe "" name,
              rideLink = shortenedTrackingUrl
            }
      void $
        Sms.sendSMS booking.merchantId booking.merchantOperatingCityId (buildSmsReq emergencyContact.mobileNumber)
          >>= Sms.checkSmsResult

checkSafetySettingConstraint :: Maybe RideShareOptions -> RiderConfig -> UTCTime -> Bool
checkSafetySettingConstraint setting riderConfig now =
  case setting of
    Just ALWAYS_SHARE -> True
    Just SHARE_WITH_TIME_CONSTRAINTS -> checkTimeConstraintForFollowRide riderConfig now
    _ -> False

checkTimeConstraintForFollowRide :: DRC.RiderConfig -> UTCTime -> Bool
checkTimeConstraintForFollowRide config now = do
  let time = timeToTimeOfDay $ utctDayTime now
  isTimeWithinBounds (secondsToTimeOfDay config.safetyCheckStartTime) (secondsToTimeOfDay config.safetyCheckEndTime) time

notifyOnStopReached ::
  ServiceFlow m r =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnStopReached booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let entity = Notification.Entity Notification.Product rideId.getId ()
  dynamicNotifyPerson
    person
    (createNotificationReq "STOP_REACHED" identity)
    EmptyDynamicParam
    entity
    booking.tripCategory
    [("driverName", driverName)]
    (Just booking.configInExperimentVersions)
    Nothing

-- title = T.pack "Stop Reached!"
-- body =
--   unwords
--     [ driverName,
--       "has reached the stop. You may add another stop!"
--     ]

getDisabilityTag :: (ServiceFlow m r) => Maybe Bool -> Id Person -> m (Maybe Text)
getDisabilityTag hasDisability personId = case hasDisability of
  Just True -> runInReplica $ fmap (.tag) <$> PD.findByPersonId personId
  _ -> return Nothing

getNotificationSound :: (ServiceFlow m r) => Maybe Text -> Maybe NSC.NotificationSoundsConfig -> m (Maybe Text)
getNotificationSound tag notificationSoundFromConfig =
  return case (tag, notificationSoundFromConfig) of
    (Just "BLIND_LOW_VISION", Just ns) -> ns.blindSound
    (_, Just ns) -> ns.defaultSound
    (_, _) -> Nothing

data NotifReq = NotifReq
  { title :: Text,
    message :: Text
  }
  deriving (Generic, ToJSON, FromJSON, Show)

notifyPersonOnEvents ::
  ServiceFlow m r =>
  Person ->
  NotifReq ->
  Notification.Category ->
  m ()
notifyPersonOnEvents person entityData notifType = do
  let merchantOperatingCityId = person.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType notifType merchantOperatingCityId
  let notificationSound = NSC.defaultSound =<< notificationSoundFromConfig
  let notificationData =
        Notification.NotificationReq
          { category = notifType,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product person.id.getId (),
            body = body,
            title = title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
      title = entityData.title
      body =
        unwords
          [ entityData.message
          ]
  notifyPerson person.merchantId merchantOperatingCityId person.id notificationData Nothing

notifyTicketCancelled :: (ServiceFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Text -> Text -> Person.Person -> m ()
notifyTicketCancelled ticketBookingId ticketBookingCategoryName person = do
  let entity = Notification.Entity Notification.Product person.id.getId ()
  dynamicNotifyPerson
    person
    (createNotificationReq "TICKET_CANCELLED" identity)
    EmptyDynamicParam
    entity
    Nothing
    [ ("ticketBookingCategoryName", ticketBookingCategoryName),
      ("ticketBookingId", ticketBookingId)
    ]
    Nothing
    Nothing

-- title = ticketBookingCategoryName <> " Ticket Service is Cancelled"
-- body =
--   unwords
--     [ "Sorry, Ticket Booking " <> ticketBookingId <> " having " <> ticketBookingCategoryName <> " Service is cancelled will be Refunded",
--       "Check the app for details"
--     ]

data FirstRideEvent = FirstRideEvent
  { vehicleCategory :: BecknEnums.VehicleCategory,
    hasTakenFirstValidRide :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyFirstRideEvent :: (ServiceFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Person -> BecknEnums.VehicleCategory -> Maybe TripCategory -> m ()
notifyFirstRideEvent personId vehicleCategory tripCategory = do
  person <- runInReplica $ Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let entity = Notification.Entity Notification.Product person.id.getId (FirstRideEvent vehicleCategory True)
  dynamicNotifyPerson
    person
    (createNotificationReq "FIRST_RIDE_EVENT" identity)
    EmptyDynamicParam
    entity
    tripCategory
    []
    Nothing
    Nothing

-- title = fromMaybe (T.pack "First Ride Event") mbTitle
-- body = fromMaybe (unwords ["Congratulations! You have taken your first ride with us."]) mbBody

notifyToAllBookingParties :: (ServiceFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => [Person] -> Maybe TripCategory -> Text -> m ()
notifyToAllBookingParties persons tripCategory notikey =
  forM_ persons \person -> do
    when (isJust person.deviceToken) $ do
      let entity = Notification.Entity Notification.Product person.id.getId ()
      dynamicNotifyPerson
        person
        (createNotificationReq notikey identity)
        EmptyDynamicParam
        entity
        tripCategory
        []
        Nothing
        Nothing

notifyOnTripUpdate ::
  ServiceFlow m r =>
  SRB.Booking ->
  SRide.Ride ->
  Maybe (Text, Text) ->
  m ()
notifyOnTripUpdate booking ride err = do
  let personId = booking.riderId
      rideId = ride.id
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  (title, body) <- case err of
    Just errorMessage -> return errorMessage
    Nothing -> do
      mbMerchantPN <- CPN.findMatchingMerchantPNInRideFlow merchantOperatingCityId "TRIP_UPDATED" booking.tripCategory Nothing person.language booking.configInExperimentVersions >>= fromMaybeM (InternalError "Trip update merchant push notification not found")
      return (mbMerchantPN.title, mbMerchantPN.body)
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.TRIP_UPDATED merchantOperatingCityId
  let notificationSound = maybe (Just "default") NSC.defaultSound notificationSoundFromConfig
  let notificationData =
        Notification.NotificationReq
          { category = Notification.TRIP_UPDATED,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product rideId.getId (),
            body,
            title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
      toLocationDestination = do
        loc <- ride.toLocation
        let addr = loc.address
        addr.building <|> addr.title <|> addr.area <|> addr.street <|> addr.city
      liveActivityReq =
        ( case person.liveActivityToken of
            Just _liveActivityToken ->
              Just $
                FCMType.LiveActivityReq
                  { liveActivityToken = _liveActivityToken,
                    liveActivityReqType = "update",
                    liveActivityNotificationType = "TRIP_UPDATED",
                    liveActivityContentState =
                      FCMType.LiveActivityContentState
                        { activityStatus = "ONRIDE",
                          driverInfo = Nothing,
                          bookingInfo =
                            Just $
                              FCMType.BookingInfo
                                { vehicleColor = ride.vehicleColor,
                                  vehicleName = Just ride.vehicleModel,
                                  vehicleNumber = Just ride.vehicleNumber,
                                  vehicleVariant = Just (show ride.vehicleVariant),
                                  source = Nothing,
                                  destination = toLocationDestination,
                                  estimatedFare = Just (show booking.estimatedFare)
                                },
                          timerDuration = Nothing,
                          customMessage = Nothing
                        },
                    liveActivityApnsPriority = "10"
                  }
            _ -> Nothing
        )
  notifyPerson person.merchantId merchantOperatingCityId person.id notificationData liveActivityReq

--"Destination and Fare Updated" "Your edit request was accepted by your driver!"

notifyAboutScheduledRide :: (ServiceFlow m r) => SRB.Booking -> Text -> Text -> m ()
notifyAboutScheduledRide booking title body = do
  person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.SCHEDULED_RIDE_REMINDER person.merchantOperatingCityId
  let notificationSound = maybe (Just "default") NSC.defaultSound notificationSoundFromConfig
      notificationData =
        Notification.NotificationReq
          { category = Notification.SCHEDULED_RIDE_REMINDER,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product booking.id.getId (),
            body,
            title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
  notifyPerson person.merchantId person.merchantOperatingCityId person.id notificationData Nothing

notifyPaymentFulfillment :: (ServiceFlow m r) => Notification.Category -> Id DOrder.PaymentOrder -> Id Person -> DOrder.PaymentServiceType -> m ()
notifyPaymentFulfillment notifCategory paymentOrderId personId paymentServiceType = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  notificationSoundFromConfig <- SQNSC.findByNotificationType notifCategory person.merchantOperatingCityId
  mbMerchantPN <- CPN.findMatchingMerchantPN person.merchantOperatingCityId (mkRefundNotificationKey) Nothing Nothing person.language Nothing
  whenJust mbMerchantPN \merchantPN -> do
    when (merchantPN.shouldTrigger) $ do
      let notificationSound = maybe (Just "default") NSC.defaultSound notificationSoundFromConfig
          templateParams = [] :: [(Text, Text)]
          title = buildTemplate templateParams merchantPN.title
          body = buildTemplate templateParams merchantPN.body
          notificationData =
            Notification.NotificationReq
              { category = notifCategory,
                subCategory = Nothing,
                showNotification = Notification.SHOW,
                messagePriority = Nothing,
                entity = Notification.Entity Notification.Product paymentOrderId.getId (),
                body,
                title,
                dynamicParams = EmptyDynamicParam,
                auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
                ttl = Nothing,
                sound = notificationSound
              }
      notifyPerson person.merchantId person.merchantOperatingCityId person.id notificationData Nothing
  where
    mkRefundNotificationKey :: Text
    mkRefundNotificationKey =
      T.pack (show paymentServiceType) <> "_" <> T.pack (show notifCategory)

getAllOtherRelatedPartyPersons :: ServiceFlow m r => SRB.Booking -> m [Person]
getAllOtherRelatedPartyPersons booking = do
  case booking.tripCategory of
    Just (Trip.Delivery _) -> do
      allBookingParties <- QBPL.findAllActiveByBookingId booking.id
      let allBookingPartyIds = filter (booking.riderId /=) $ map (.partyId) allBookingParties
      allParty <- catMaybes <$> mapM Person.findById allBookingPartyIds
      pure $ filter (isJust . (.deviceToken)) allParty
    _ -> pure []

mkSafetyNotificationKey :: T.Text -> Text
mkSafetyNotificationKey code =
  case readMaybe (T.unpack code) of
    Just BecknEnums.DEVIATION -> "SAFETY_ALERT_DEVIATION"
    Just BecknEnums.RIDE_STOPPAGE -> "SAFETY_ALERT_RIDE_STOPPAGE"
    Nothing -> code

notifyAboutDeletedPerson :: (ServiceFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Person -> m ()
notifyAboutDeletedPerson personId = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let entity = Notification.Entity Notification.Product person.id.getId ()
  dynamicNotifyPerson
    person
    (createNotificationReq "ACCOUNT_DELETED" identity)
    EmptyDynamicParam
    entity
    Nothing
    []
    Nothing
    Nothing

notifyOnRideEndOffer ::
  ServiceFlow m r =>
  Person ->
  m ()
notifyOnRideEndOffer person = do
  let entity = Notification.Entity Notification.Person person.id.getId ()
  dynamicNotifyPerson
    person
    (createNotificationReq "RIDE_END_OFFER" identity)
    EmptyDynamicParam
    entity
    Nothing
    []
    Nothing
    Nothing

notifyRefunds ::
  ServiceFlow m r =>
  DRefundRequest.RefundRequest ->
  m ()
notifyRefunds refundRequest = case refundRequest.status of
  DRefundRequest.APPROVED -> notifyRefunds' FCMType.REFUND_REQUEST_APPROVED refundRequest
  DRefundRequest.REJECTED -> notifyRefunds' FCMType.REFUND_REQUEST_REJECTED refundRequest
  DRefundRequest.FAILED -> notifyRefunds' FCMType.REFUND_FAILED refundRequest
  DRefundRequest.REFUNDED -> notifyRefunds' FCMType.REFUND_SUCCESS refundRequest
  DRefundRequest.OPEN -> pure ()

notifyRefunds' ::
  ServiceFlow m r =>
  FCMType.FCMNotificationType ->
  DRefundRequest.RefundRequest ->
  m ()
notifyRefunds' notificationType DRefundRequest.RefundRequest {..} = do
  person <- runInReplica $ Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let entity = Notification.Entity Notification.Product personId.getId ()
  dynamicNotifyPerson
    person
    (createNotificationReq (show notificationType) identity)
    EmptyDynamicParam
    entity
    Nothing
    [ ("rideId", orderId.getId), -- should we use rideShortId?
      ("refundPurpose", show refundPurpose),
      ("status", show status),
      ("code", code.getRefundRequestCode),
      ("requestedAmount", Price.showPriceWithRounding . Price.mkPrice (Just currency) $ fromMaybe transactionAmount requestedAmount),
      ("transactionAmount", Price.showPriceWithRounding $ Price.mkPrice (Just currency) transactionAmount),
      ("refundsAmount", Price.showPriceWithRounding . Price.mkPrice (Just currency) $ fromMaybe 0.0 refundsAmount)
    ]
    Nothing
    Nothing
