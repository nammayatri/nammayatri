{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Domain.Action.Beckn.Common
  ( module Domain.Action.Beckn.Common,
    module Reexport,
  )
where

import qualified BecknV2.OnDemand.Enums as BecknEnums
import qualified BecknV2.OnDemand.Utils.Common as Utils
import Control.Monad.Extra (mapMaybeM)
import qualified Data.Geohash as Geohash
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import Data.Either.Extra (eitherToMaybe)
import Data.Time hiding (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Domain.Action.UI.Cancel (makeCustomerBlockingKey)
import Domain.Action.UI.HotSpot
import Domain.Action.UI.RidePayment as Reexport
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.BookingStatus as BT
import qualified Domain.Types.BookingStatus as DRB
import qualified Domain.Types.Client as DC
import qualified Domain.Types.ClientPersonInfo as DPCI
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.FareBreakup as DFareBreakup
import Domain.Types.HotSpot
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.PersonStats as DPS
import qualified Domain.Types.RecentLocation as DTRL
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideRelatedNotificationConfig as DRN
import qualified Domain.Types.RideStatus as DRide
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.Trip as Trip
import qualified Domain.Types.VehicleVariant as DV
import qualified Domain.Types.Yudhishthira as Y
import Environment
import Kernel.Beam.Functions as B
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.External.Encryption
import Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Payout.Types as PT
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Confidence
import qualified Kernel.Types.Flow
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SW
import Kernel.Types.Version
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import qualified Kernel.Utils.Time as KUT
import qualified Lib.Payment.Domain.Action as Payout
import qualified Lib.Payment.Domain.Types.Common as DLP
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import Lib.SessionizerMetrics.Types.Event
import qualified Lib.Yudhishthira.Event as Yudhishthira
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types as Yudhishthira
import qualified SharedLogic.BehaviourManagement.CustomerCancellationRate as CCR
import SharedLogic.Booking
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.Insurance as SI
import SharedLogic.JobScheduler
import qualified SharedLogic.MerchantConfig as SMC
import qualified SharedLogic.MessageBuilder as MessageBuilder
import SharedLogic.Payment as SPayment
import qualified SharedLogic.ScheduledNotifications as SN
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.BppDetails as CQBPP
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.MerchantConfig as CMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as CRRN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.ClientPersonInfo as QCP
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyLeg as QJL
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonStats as QPersonStats
import qualified Storage.Queries.RecentLocation as SQRL
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideExtra as QERIDE
import Tools.Constants
import Tools.Error
import Tools.Event
import Tools.Maps (LatLong (..))
import Tools.Metrics (HasBAPMetrics, incrementRideCreatedRequestCount)
import qualified Tools.Notifications as Notify
import qualified Tools.Payout as TP
import qualified Tools.SMS as Sms
import qualified Tools.Whatsapp as Whatsapp
import TransactionLogs.Types
import qualified UrlShortner.Common as UrlShortner

data BookingDetails = BookingDetails
  { bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    driverName :: Text,
    driverImage :: Maybe Text,
    driverMobileNumber :: Text,
    driverAlternatePhoneNumber :: Maybe Text,
    driverMobileCountryCode :: Maybe Text,
    driverRating :: Maybe Centesimal,
    driverRegisteredAt :: Maybe UTCTime,
    vehicleNumber :: Text,
    vehicleColor :: Maybe Text,
    vehicleModel :: Text,
    otp :: Text,
    isInitiatedByCronJob :: Bool
  }

data RideAssignedReq = RideAssignedReq
  { bookingDetails :: BookingDetails,
    transactionId :: Text,
    isDriverBirthDay :: Bool,
    vehicleAge :: Maybe Months,
    isFreeRide :: Bool,
    driverAccountId :: Maybe Payment.AccountId,
    previousRideEndPos :: Maybe LatLong,
    isAlreadyFav :: Bool,
    favCount :: Maybe Int,
    fareBreakups :: Maybe [DFareBreakup],
    driverTrackingUrl :: Maybe BaseUrl,
    isSafetyPlus :: Bool
  }

data OnlinePaymentParameters = OnlinePaymentParameters
  { paymentMethodId :: Payment.PaymentMethodId,
    customerPaymentId :: Payment.CustomerId,
    driverAccountId :: Payment.AccountId,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    email :: Maybe Text
  }

data ValidatedRideAssignedReq = ValidatedRideAssignedReq
  { bookingDetails :: BookingDetails,
    isDriverBirthDay :: Bool,
    isFreeRide :: Bool,
    vehicleAge :: Maybe Months,
    onlinePaymentParameters :: Maybe OnlinePaymentParameters,
    previousRideEndPos :: Maybe LatLong,
    booking :: DRB.Booking,
    fareBreakups :: Maybe [DFareBreakup],
    driverTrackingUrl :: Maybe BaseUrl,
    isAlreadyFav :: Bool,
    favCount :: Maybe Int,
    isSafetyPlus :: Bool
  }

data RideStartedReq = RideStartedReq
  { bookingDetails :: BookingDetails,
    transactionId :: Text,
    tripStartLocation :: Maybe LatLong,
    endOtp_ :: Maybe Text,
    startOdometerReading :: Maybe Centesimal,
    rideStartTime :: Maybe UTCTime,
    driverArrivalTime :: Maybe UTCTime,
    estimatedEndTimeRangeStart :: Maybe UTCTime,
    estimatedEndTimeRangeEnd :: Maybe UTCTime
  }

data ValidatedRideStartedReq = ValidatedRideStartedReq
  { bookingDetails :: BookingDetails,
    tripStartLocation :: Maybe LatLong,
    endOtp_ :: Maybe Text,
    startOdometerReading :: Maybe Centesimal,
    rideStartTime :: Maybe UTCTime,
    driverArrivalTime :: Maybe UTCTime,
    ride :: DRide.Ride,
    booking :: DRB.Booking,
    estimatedEndTimeRange :: Maybe DRide.EstimatedEndTimeRange
  }

data RideCompletedReq = RideCompletedReq
  { bookingDetails :: BookingDetails,
    transactionId :: Text,
    fare :: Price,
    totalFare :: Price,
    fareBreakups :: [DFareBreakup],
    chargeableDistance :: Maybe HighPrecMeters,
    traveledDistance :: Maybe HighPrecMeters,
    tollConfidence :: Maybe Confidence,
    paymentUrl :: Maybe Text,
    tripEndLocation :: Maybe LatLong,
    endOdometerReading :: Maybe Centesimal,
    rideEndTime :: Maybe UTCTime,
    paymentStatus :: Maybe DRB.PaymentStatus,
    isValidRide :: Maybe Bool
  }

data ValidatedRideCompletedReq = ValidatedRideCompletedReq
  { bookingDetails :: BookingDetails,
    fare :: Price,
    totalFare :: Price,
    fareBreakups :: [DFareBreakup],
    chargeableDistance :: Maybe HighPrecMeters,
    traveledDistance :: Maybe HighPrecMeters,
    tollConfidence :: Maybe Confidence,
    paymentUrl :: Maybe Text,
    tripEndLocation :: Maybe LatLong,
    endOdometerReading :: Maybe Centesimal,
    rideEndTime :: Maybe UTCTime,
    booking :: DRB.Booking,
    ride :: DRide.Ride,
    person :: DPerson.Person,
    paymentStatus :: Maybe DRB.PaymentStatus,
    isValidRide :: Maybe Bool
  }

data ValidatedFarePaidReq = ValidatedFarePaidReq
  { booking :: DRB.Booking,
    paymentStatus :: DRB.PaymentStatus
  }

data BookingCancelledReq = BookingCancelledReq
  { bookingDetails :: Maybe BookingDetails,
    bppBookingId :: Id DRB.BPPBooking,
    cancellationSource :: DBCR.CancellationSource,
    transactionId :: Text
  }

data ValidatedBookingCancelledReq = ValidatedBookingCancelledReq
  { bookingDetails :: Maybe BookingDetails,
    bppBookingId :: Id DRB.BPPBooking,
    cancellationSource :: DBCR.CancellationSource,
    transactionId :: Text,
    booking :: DRB.Booking,
    mbRide :: Maybe DRide.Ride
  }

data BookingReallocationReq = BookingReallocationReq ----need to use in future
  { bookingDetails :: BookingDetails,
    reallocationSource :: DBCR.CancellationSource,
    transactionId :: Text
  }

data ValidatedBookingReallocationReq = ValidatedBookingReallocationReq ----need to use in future
  { bookingDetails :: BookingDetails,
    reallocationSource :: DBCR.CancellationSource,
    booking :: DRB.Booking,
    ride :: DRide.Ride
  }

data DriverArrivedReq = DriverArrivedReq
  { bookingDetails :: BookingDetails,
    arrivalTime :: Maybe UTCTime,
    transactionId :: Text
  }

data ValidatedDriverArrivedReq = ValidatedDriverArrivedReq
  { bookingDetails :: BookingDetails,
    arrivalTime :: Maybe UTCTime,
    ride :: DRide.Ride,
    booking :: DRB.Booking
  }

buildRide :: (MonadFlow m, EncFlow m r, HasFlowEnv m r '["version" ::: DeploymentVersion], HasFlowEnv m r '["isMetroTestTransaction" ::: Bool], HasFlowEnv m r '["cloudType" ::: Maybe CloudType]) => ValidatedRideAssignedReq -> Maybe DMerchant.Merchant -> UTCTime -> DRide.RideStatus -> m DRide.Ride
buildRide req@ValidatedRideAssignedReq {..} mbMerchant now status = do
  let BookingDetails {..} = bookingDetails
  guid <- generateGUID
  shortId <- generateShortId
  deploymentVersion <- asks (.version)
  driverPhoneNumber <- mapM encrypt (Just bookingDetails.driverMobileNumber)
  driverAlternateNumber <- mapM encrypt bookingDetails.driverAlternatePhoneNumber
  let fromLocation = booking.fromLocation
      (toLocation, stops) = case booking.bookingDetails of
        DRB.OneWayDetails details -> (Just details.toLocation, details.stops)
        DRB.RentalDetails _ -> (Nothing, [])
        DRB.DriverOfferDetails details -> (Just details.toLocation, details.stops)
        DRB.OneWaySpecialZoneDetails details -> (Just details.toLocation, details.stops)
        DRB.InterCityDetails details -> (Just details.toLocation, [])
        DRB.AmbulanceDetails details -> (Just details.toLocation, [])
        DRB.DeliveryDetails details -> (Just details.toLocation, [])
        DRB.MeterRideDetails details -> (details.toLocation, [])
      allowedEditLocationAttempts = Just $ maybe 0 (.numOfAllowedEditPickupLocationAttemptsThreshold) mbMerchant
      allowedEditPickupLocationAttempts = Just $ maybe 0 (.numOfAllowedEditPickupLocationAttemptsThreshold) mbMerchant
      onlinePayment = SPayment.isOnlinePayment mbMerchant booking
  mfuPattern <- fromMaybeM (MerchantDoesNotExist ("BuildRide merchant:" <> booking.merchantId.getId)) (fmap DMerchant.mediaFileUrlPattern mbMerchant)
  let fileUrl =
        ( mfuPattern
            & Text.replace "<DOMAIN>" "driver/photo"
            & flip (Text.replace "<FILE_PATH>")
        )
          <$> bookingDetails.driverImage
  isMetroTestTransaction <- asks (.isMetroTestTransaction)
  cloudType <- asks (.cloudType)
  return
    DRide.Ride
      { id = guid,
        bookingId = booking.id,
        shortId = bool shortId (ShortId $ "test-" <> shortId.getShortId) isMetroTestTransaction,
        merchantId = Just booking.merchantId,
        merchantOperatingCityId = Just booking.merchantOperatingCityId,
        clientId = booking.clientId,
        trackingUrl = req.driverTrackingUrl,
        fare = Nothing,
        totalFare = Nothing,
        pickupSpeedInMPS = Nothing,
        chargeableDistance = Nothing,
        traveledDistance = Nothing,
        driverArrivalTime = Nothing,
        vehicleVariant = DV.castServiceTierToVariant booking.vehicleServiceTierType, -- fix later
        vehicleServiceTierType = Just booking.vehicleServiceTierType,
        createdAt = now,
        updatedAt = now,
        rideStartTime = Nothing,
        rideEndTime = Nothing,
        rideRating = Nothing,
        safetyCheckStatus = Nothing,
        isFreeRide = Just isFreeRide,
        endOtp = Nothing,
        isPetRide = booking.isPetRide,
        startOdometerReading = Nothing,
        endOdometerReading = Nothing,
        clientBundleVersion = booking.clientBundleVersion,
        clientSdkVersion = booking.clientSdkVersion,
        clientDevice = booking.clientDevice,
        clientConfigVersion = booking.clientConfigVersion,
        backendConfigVersion = booking.backendConfigVersion,
        backendAppVersion = Just deploymentVersion.getDeploymentVersion,
        driversPreviousRideDropLoc = previousRideEndPos,
        showDriversPreviousRideDropLoc = isJust previousRideEndPos,
        feedbackSkipped = False,
        tollConfidence = Nothing,
        distanceUnit = booking.distanceUnit,
        driverAccountId = req.onlinePaymentParameters <&> (.driverAccountId),
        paymentStatus = DRide.NotInitiated,
        refundRequestStatus = Nothing,
        vehicleAge = req.vehicleAge,
        cancellationFeeIfCancelled = Nothing,
        isAlreadyFav = Just isAlreadyFav,
        safetyJourneyStatus = Nothing,
        driverImage = fileUrl,
        destinationReachedAt = Nothing,
        estimatedEndTimeRange = Nothing,
        tipAmount = Nothing,
        hasStops = booking.hasStops,
        billingCategory = booking.billingCategory,
        wasRideSafe = Nothing,
        pickupRouteCallCount = Just 0,
        talkedWithDriver = Nothing,
        isSafetyPlus = isSafetyPlus,
        isInsured = booking.isInsured,
        insuredAmount = booking.insuredAmount,
        cancellationChargesOnCancel = Nothing,
        pickupEtaLogicVersion = Nothing,
        -- Commission is provider-side data, calculated on BPP. On BAP side, it remains Nothing.
        -- If commission is needed on BAP, it should be calculated here or received from BPP.
        commission = booking.commission,
        cloudType = cloudType,
        ..
      }

rideAssignedReqHandler ::
  ( HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl, "nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig, "version" ::: DeploymentVersion, "cloudType" ::: Maybe CloudType],
    HasField "storeRidesTimeLimit" r Int,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    SchedulerFlow r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasKafkaProducer r,
    HasFlowEnv m r '["isMetroTestTransaction" ::: Bool],
    HasField "blackListedJobs" r [Text]
  ) =>
  ValidatedRideAssignedReq ->
  m ()
rideAssignedReqHandler req = do
  let BookingDetails {..} = req.bookingDetails
  void $ QRB.updateBPPBookingId req.booking.id bppBookingId
  let booking = req.booking {DRB.bppBookingId = Just bppBookingId}
  mbMerchant <- CQM.findById booking.merchantId
  now <- getCurrentTime
  let rideStatus = case mbMerchant of
        Just merchant | (maybe True not booking.isMultimodalSearch) && diffUTCTime booking.startTime now > merchant.scheduleRideBufferTime -> DRide.UPCOMING
        _ -> DRide.NEW
  mbRide <- QRide.findByBPPRideId bppRideId
  case mbRide of
    Just ride -> do
      QERIDE.updateStatus ride.id rideStatus
      QERIDE.updateIsSafetyPlus ride.id req.isSafetyPlus
      unless isInitiatedByCronJob $ do
        Notify.notifyOnRideAssigned booking ride
        when req.isDriverBirthDay $
          Notify.notifyDriverBirthDay booking.riderId booking.tripCategory driverName
      withLongRetry $ CallBPP.callTrack booking ride
    Nothing -> assignRideUpdate req mbMerchant rideStatus now
  where
    notifyRideRelatedNotificationOnEvent booking ride now timeDiffEvent = do
      rideRelatedNotificationConfigList <- CRRN.findAllByMerchantOperatingCityIdAndTimeDiffEventInRideFlow booking.merchantOperatingCityId timeDiffEvent booking.configInExperimentVersions
      forM_ rideRelatedNotificationConfigList (SN.pushReminderUpdatesInScheduler booking ride now)
    assignRideUpdate ::
      ( HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl, "nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig, "version" ::: DeploymentVersion, "cloudType" ::: Maybe CloudType],
        HasField "storeRidesTimeLimit" r Int,
        CacheFlow m r,
        EsqDBFlow m r,
        MonadFlow m,
        EncFlow m r,
        EsqDBReplicaFlow m r,
        HasLongDurationRetryCfg r c,
        HasShortDurationRetryCfg r c,
        HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
        SchedulerFlow r,
        HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
        HasBAPMetrics m r,
        EventStreamFlow m r,
        HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
        HasKafkaProducer r,
        HasFlowEnv m r '["isMetroTestTransaction" ::: Bool],
        HasField "blackListedJobs" r [Text]
      ) =>
      ValidatedRideAssignedReq ->
      Maybe DMerchant.Merchant ->
      DRide.RideStatus ->
      UTCTime ->
      m ()
    assignRideUpdate req'@ValidatedRideAssignedReq {..} mbMerchant rideStatus now = do
      let BookingDetails {..} = req'.bookingDetails
      ride <- buildRide req' mbMerchant now rideStatus
      let applicationFeeAmount = fromMaybe 0 booking.commission
      whenJust req'.onlinePaymentParameters $ \OnlinePaymentParameters {..} -> do
        let createPaymentIntentReq =
              Payment.CreatePaymentIntentReq
                { orderShortId = ride.shortId.getShortId,
                  amount = booking.estimatedFare.amount,
                  applicationFeeAmount,
                  currency = booking.estimatedFare.currency,
                  customer = customerPaymentId,
                  paymentMethod = paymentMethodId,
                  receiptEmail = email,
                  driverAccountId
                }
        handle (SPayment.paymentErrorHandler booking) $ withShortRetry (void $ SPayment.makePaymentIntent booking.merchantId merchantOperatingCityId booking.paymentMode booking.riderId ride createPaymentIntentReq)
      triggerRideCreatedEvent RideEventData {ride = ride, personId = booking.riderId, merchantId = booking.merchantId}
      let category = case booking.specialLocationTag of
            Just _ -> "specialLocation"
            Nothing -> "normal"
      incrementRideCreatedRequestCount booking.merchantId.getId booking.merchantOperatingCityId.getId category
      QRB.updateStatus booking.id DRB.TRIP_ASSIGNED
      QRide.createRide ride
      QPFS.clearCache booking.riderId
      fork "Increment assigned count for customer cancellation rate" $ do
        windowSize <- CCR.getWindowSize booking.merchantOperatingCityId
        void $ CCR.incrementAssignedCount booking.riderId windowSize
      unless isInitiatedByCronJob $ do
        if rideStatus == DRide.UPCOMING then Notify.notifyOnScheduledRideAccepted booking ride else Notify.notifyOnRideAssigned booking ride
        when req'.isDriverBirthDay $ do
          Notify.notifyDriverBirthDay booking.riderId booking.tripCategory driverName
      withLongRetry $ CallBPP.callTrack booking ride

      notifyRideRelatedNotificationOnEvent booking ride now DRN.RIDE_ASSIGNED
      notifyRideRelatedNotificationOnEvent booking ride now DRN.PICKUP_TIME

      riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow booking.merchantOperatingCityId booking.configInExperimentVersions >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
      when (booking.isDashboardRequest == Just True && riderConfig.autoSendBookingDetailsViaWhatsapp == Just True) $ do
        fork "Sending Dashboard Ride Flow Booking Details" $ do
          sendRideBookingDetailsViaWhatsapp booking.riderId ride booking riderConfig

      -- Notify sender of delivery booking
      when (booking.tripCategory == Just (Trip.Delivery Trip.OneWayOnDemandDynamicOffer)) $ do
        fork "Sending Delivery Details SMS to Sender And Receiver" $ do
          mbExoPhone <- CQExophone.findByPrimaryPhone booking.primaryExophone
          senderParty <- QBPL.findOneActiveByBookingIdAndTripParty booking.id (Trip.DeliveryParty Trip.Sender) >>= fromMaybeM (InternalError $ "Sender booking party not found for " <> booking.id.getId)
          receiverParty <- QBPL.findOneActiveByBookingIdAndTripParty booking.id (Trip.DeliveryParty Trip.Receiver) >>= fromMaybeM (InternalError $ "Receiver booking party not found for " <> booking.id.getId)
          senderPerson <- QP.findById senderParty.partyId >>= fromMaybeM (PersonDoesNotExist senderParty.partyId.getId)
          receiverPerson <- QP.findById receiverParty.partyId >>= fromMaybeM (PersonDoesNotExist receiverParty.partyId.getId)
          encSenderMobileNumber <- senderPerson.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
          encReceiverMobileNumber <- receiverPerson.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
          let exophoneNumber =
                maybe booking.primaryExophone (\exophone -> if not exophone.isPrimaryDown then exophone.primaryPhone else exophone.backupPhone) mbExoPhone
          let trackLink = Notify.buildTrackingUrl ride.id [("vp", "delivery")] riderConfig.trackingShortUrlPattern
          let senderSmsReq =
                MessageBuilder.BuildDeliveryMessageReq
                  { MessageBuilder.driverName = ride.driverName,
                    MessageBuilder.driverNumber = exophoneNumber,
                    MessageBuilder.appUrl = riderConfig.appUrl,
                    MessageBuilder.senderName = senderParty.partyName,
                    MessageBuilder.receiverName = receiverParty.partyName,
                    MessageBuilder.trackingUrl = Just trackLink,
                    MessageBuilder.otp = ride.otp,
                    MessageBuilder.hasEnded = False,
                    MessageBuilder.pickedUp = False,
                    MessageBuilder.deliveryMessageType = MessageBuilder.SenderReq
                  }
              receiverSmsReq = senderSmsReq {MessageBuilder.deliveryMessageType = MessageBuilder.ReceiverReq}
          sbuildSmsReq <- MessageBuilder.buildDeliveryDetailsMessage booking.merchantOperatingCityId senderSmsReq
          rbuildSmsReq <- MessageBuilder.buildDeliveryDetailsMessage booking.merchantOperatingCityId receiverSmsReq
          senderMobileNumber <- decrypt encSenderMobileNumber
          receiverMobileNumber <- decrypt encReceiverMobileNumber
          let sphoneNumber = (fromMaybe "+91" senderPerson.mobileCountryCode) <> senderMobileNumber
              rphoneNumber = (fromMaybe "+91" receiverPerson.mobileCountryCode) <> receiverMobileNumber
          Sms.sendSMS booking.merchantId booking.merchantOperatingCityId (sbuildSmsReq sphoneNumber) >>= Sms.checkSmsResult
          Sms.sendSMS booking.merchantId booking.merchantOperatingCityId (rbuildSmsReq rphoneNumber) >>= Sms.checkSmsResult

rideStartedReqHandler ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds,
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasField "blackListedJobs" r [Text]
  ) =>
  ValidatedRideStartedReq ->
  m ()
rideStartedReqHandler ValidatedRideStartedReq {..} = do
  let BookingDetails {..} = bookingDetails
  fork "ride start geohash frequencyUpdater" $ do
    case tripStartLocation of
      Just location -> frequencyUpdator booking.merchantId location Nothing TripStart Nothing
      Nothing -> return ()
  let updRideForStartReq =
        ride{status = DRide.INPROGRESS,
             rideStartTime,
             rideEndTime = Nothing,
             endOtp = endOtp_,
             driverArrivalTime,
             startOdometerReading,
             estimatedEndTimeRange
            }
  triggerRideStartedEvent RideEventData {ride = updRideForStartReq, personId = booking.riderId, merchantId = booking.merchantId}
  _ <- QRide.updateMultiple updRideForStartReq.id updRideForStartReq
  QPFS.clearCache booking.riderId
  when (updRideForStartReq.isInsured) $ fork "create insurance" $ SI.createInsurance updRideForStartReq
  now <- getCurrentTime
  rideRelatedNotificationConfigList <- CRRN.findAllByMerchantOperatingCityIdAndTimeDiffEventInRideFlow booking.merchantOperatingCityId DRN.START_TIME booking.configInExperimentVersions
  forM_ rideRelatedNotificationConfigList (SN.pushReminderUpdatesInScheduler booking updRideForStartReq (fromMaybe now rideStartTime))
  unless isInitiatedByCronJob $ do
    fork "notify emergency contacts" $ Notify.notifyRideStartToEmergencyContacts booking ride
    Notify.notifyOnRideStarted booking ride
  case booking.bookingDetails of
    DRB.RentalDetails _ -> when (booking.isDashboardRequest == Just True) sendRideEndOTPMessage
    DRB.InterCityDetails _ -> when (booking.isDashboardRequest == Just True) sendRideEndOTPMessage
    DRB.DeliveryDetails _ -> do
      deliveryInitiatedAs <- fromMaybeM (InternalError "DeliveryInitiatedBy not found") booking.initiatedBy
      when (deliveryInitiatedAs /= Trip.DeliveryParty Trip.Receiver) $ sendDeliveryDetailsToReceiver
    _ -> pure ()
  where
    sendDeliveryDetailsToReceiver = fork "Sending Delivery Details SMS to Receiver" $ do
      riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow booking.merchantOperatingCityId booking.configInExperimentVersions >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
      mbExoPhone <- CQExophone.findByPrimaryPhone booking.primaryExophone
      senderParty <- QBPL.findOneActiveByBookingIdAndTripParty booking.id (Trip.DeliveryParty Trip.Sender) >>= fromMaybeM (InternalError $ "Sender booking party not found for " <> booking.id.getId)
      receiverParty <- QBPL.findOneActiveByBookingIdAndTripParty booking.id (Trip.DeliveryParty Trip.Receiver) >>= fromMaybeM (InternalError $ "Receiver booking party not found for " <> booking.id.getId)
      receiverPerson <- QP.findById receiverParty.partyId >>= fromMaybeM (PersonDoesNotExist receiverParty.partyId.getId)
      receiverMobileNumber <- mapM decrypt receiverPerson.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
      let countryCode = fromMaybe "+91" receiverPerson.mobileCountryCode
      let phoneNumber = countryCode <> receiverMobileNumber
      endOtp <- fromMaybeM (InternalError "EndOtp not found to be send in sms for delivery receiver") endOtp_
      let trackLink = Notify.buildTrackingUrl ride.id [("vp", "delivery")] riderConfig.trackingShortUrlPattern
      let exophoneNumber =
            maybe booking.primaryExophone (\exophone -> if not exophone.isPrimaryDown then exophone.primaryPhone else exophone.backupPhone) mbExoPhone
      let receiverSmsReq =
            MessageBuilder.BuildDeliveryMessageReq
              { MessageBuilder.driverName = ride.driverName,
                MessageBuilder.driverNumber = exophoneNumber,
                MessageBuilder.trackingUrl = Just trackLink,
                MessageBuilder.appUrl = riderConfig.appUrl,
                MessageBuilder.senderName = senderParty.partyName,
                MessageBuilder.receiverName = receiverParty.partyName,
                MessageBuilder.otp = endOtp,
                MessageBuilder.hasEnded = False,
                MessageBuilder.pickedUp = True,
                MessageBuilder.deliveryMessageType = MessageBuilder.ReceiverReq
              }
      buildSmsReq <- MessageBuilder.buildDeliveryDetailsMessage booking.merchantOperatingCityId receiverSmsReq
      Sms.sendSMS booking.merchantId booking.merchantOperatingCityId (buildSmsReq phoneNumber) >>= Sms.checkSmsResult

    sendRideEndOTPMessage = fork "sending ride end otp sms" $ do
      let merchantOperatingCityId = booking.merchantOperatingCityId
      merchantConfig <- QMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
      if merchantConfig.enableDashboardSms
        then do
          case endOtp_ of
            Just endOtp' -> do
              customer <- B.runInReplica $ QP.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
              mobileNumber <- mapM decrypt customer.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
              let countryCode = fromMaybe "+91" customer.mobileCountryCode
              let phoneNumber = countryCode <> mobileNumber
              buildSmsReq <-
                MessageBuilder.buildSendRideEndOTPMessage merchantOperatingCityId $
                  MessageBuilder.BuildSendRideEndOTPMessageReq
                    { otp = show endOtp'
                    }
              Sms.sendSMS booking.merchantId merchantOperatingCityId (buildSmsReq phoneNumber) >>= Sms.checkSmsResult
            _ -> pure ()
        else do
          logInfo "Merchant not configured to send dashboard sms"
          pure ()

data RideEndOffersKafkaData = RideEndOffersKafkaData
  { rideId :: Id DRide.Ride,
    personId :: Id DPerson.Person,
    tags :: [LYT.TagNameValueExpiry],
    createdAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON)

rideCompletedReqHandler ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    ClickhouseFlow m r,
    SchedulerFlow r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance),
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    HasField "blackListedJobs" r [Text]
  ) =>
  ValidatedRideCompletedReq ->
  m ()
rideCompletedReqHandler ValidatedRideCompletedReq {..} = do
  let BookingDetails {..} = bookingDetails
  fork "ride end geohash frequencyUpdater" $ do
    case tripEndLocation of
      Just location -> frequencyUpdator booking.merchantId location Nothing TripEnd Nothing
      Nothing -> return ()
  fork "updating total rides count" $ SMC.updateTotalRidesCounters person
  merchantConfigs <- CMC.findAllByMerchantOperatingCityIdInRideFlow booking.merchantOperatingCityId booking.configInExperimentVersions
  SMC.updateTotalRidesInWindowCounters booking.riderId merchantConfigs
  mbDriverPhoneNumber <- mapM decrypt ride.driverPhoneNumber
  let driverPhoneNumber = fromMaybe driverMobileNumber mbDriverPhoneNumber
  mbAdvRide <- QRide.findLatestByDriverPhoneNumber driverPhoneNumber
  mbMerchant <- CQM.findById booking.merchantId
  whenJust mbAdvRide $ do \advRide -> when (advRide.id /= ride.id) $ QRide.updateshowDriversPreviousRideDropLoc False advRide.id
  let distanceUnit = ride.distanceUnit
  let updRide =
        ride{status = DRide.COMPLETED,
             fare = Just fare,
             totalFare = Just totalFare,
             chargeableDistance = convertHighPrecMetersToDistance distanceUnit <$> chargeableDistance,
             traveledDistance = convertHighPrecMetersToDistance distanceUnit <$> traveledDistance,
             tollConfidence,
             rideEndTime,
             paymentStatus = if SPayment.isOnlinePayment mbMerchant booking then DRide.NotInitiated else DRide.Completed,
             endOdometerReading,
             -- Commission is provider-side data. On BAP side, booking.commission will be Nothing.
             -- If commission is needed, it should be calculated here or received from BPP.
             commission = booking.commission
            }
  breakups <- traverse (buildFareBreakup ride.id) fareBreakups
  minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
  let shouldUpdateRideComplete =
        case minTripDistanceForReferralCfg of
          Just distance -> updRide.chargeableDistance >= Just distance && not person.hasTakenValidRide
          Nothing -> True
  riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow booking.merchantOperatingCityId booking.configInExperimentVersions >>= fromMaybeM (RiderConfigNotFound booking.merchantOperatingCityId.getId)
  fork "update first ride info" $ do
    mbPersonFirstRideInfo <- QCP.findByPersonIdAndVehicleCategory booking.riderId $ Just (Utils.mapServiceTierToCategory booking.vehicleServiceTierType)
    case mbPersonFirstRideInfo of
      Just personFirstRideInfo -> do
        QCP.updateHasTakenValidRideCount (personFirstRideInfo.rideCount + 1) booking.riderId $ Just (Utils.mapServiceTierToCategory booking.vehicleServiceTierType)
      Nothing -> do
        totalCount <- B.runInReplica $ QRB.findCountByRideIdStatusAndVehicleServiceTierType booking.riderId BT.COMPLETED (Utils.getListOfServiceTireTypes $ Utils.mapServiceTierToCategory booking.vehicleServiceTierType)
        personClientInfo <- buildPersonClientInfo booking.riderId booking.clientId booking.merchantOperatingCityId booking.merchantId (Utils.mapServiceTierToCategory booking.vehicleServiceTierType) (totalCount + 1)
        QCP.create personClientInfo
        when (totalCount == 0) $ do
          Notify.notifyFirstRideEvent booking.riderId (Utils.mapServiceTierToCategory booking.vehicleServiceTierType) booking.tripCategory
          fork ("processing referral payouts for ride: " <> ride.id.getId) $ do
            customerReferralPayout ride isValidRide riderConfig person booking.merchantId booking.merchantOperatingCityId

  when (riderConfig.enableRideEndOffers) $ do
    fork "computing offers namma tag" $
      addOffersNammaTags updRide person

  -- we should create job for collecting money from customer
  let onlinePayment = SPayment.isOnlinePayment mbMerchant booking
  when onlinePayment $ do
    let applicationFeeAmount = fromMaybe 0 booking.commission
    let scheduleAfter = riderConfig.executePaymentDelay
        executePaymentIntentJobData = ExecutePaymentIntentJobData {personId = person.id, rideId = ride.id, fare = totalFare, applicationFeeAmount = applicationFeeAmount}
    logDebug $ "Update payment intent for order: " <> ride.id.getId
    (customerPaymentId, paymentMethodId) <- SPayment.getCustomerAndPaymentMethod booking person
    driverAccountId <- ride.driverAccountId & fromMaybeM (RideFieldNotPresent "driverAccountId")
    email <- mapM decrypt person.email
    let createPaymentIntentReq =
          Payment.CreatePaymentIntentReq
            { orderShortId = ride.shortId.getShortId,
              amount = totalFare.amount,
              applicationFeeAmount,
              currency = totalFare.currency,
              customer = customerPaymentId,
              paymentMethod = paymentMethodId,
              receiptEmail = email,
              driverAccountId
            }
    handle (SPayment.paymentErrorHandler booking) $ withShortRetry (void $ SPayment.makePaymentIntent person.merchantId person.merchantOperatingCityId booking.paymentMode person.id ride createPaymentIntentReq)
    logDebug $ "Scheduling execute payment intent job for order: " <> show scheduleAfter
    createJobIn @_ @'ExecutePaymentIntent (Just booking.merchantId) (Just booking.merchantOperatingCityId) scheduleAfter (executePaymentIntentJobData :: ExecutePaymentIntentJobData)

  triggerRideEndEvent RideEventData {ride = updRide, personId = booking.riderId, merchantId = booking.merchantId}
  triggerBookingCompletedEvent BookingEventData {booking = booking{status = DRB.COMPLETED}}
  when shouldUpdateRideComplete $ void $ QP.updateHasTakenValidRide booking.riderId
  otherParties <- Notify.getAllOtherRelatedPartyPersons booking
  unless (booking.status == DRB.COMPLETED) $
    void $ do
      sendRideEndMessage booking
      QRB.updateStatus booking.id DRB.COMPLETED
      QBPL.makeAllInactiveByBookingId booking.id
  now <- getCurrentTime
  rideRelatedNotificationConfigList <- CRRN.findAllByMerchantOperatingCityIdAndTimeDiffEventInRideFlow booking.merchantOperatingCityId DRN.END_TIME booking.configInExperimentVersions
  forM_ rideRelatedNotificationConfigList (SN.pushReminderUpdatesInScheduler booking updRide (fromMaybe now rideEndTime))
  when (isJust paymentStatus && booking.paymentStatus /= Just DRB.PAID) $ QRB.updatePaymentStatus booking.id (fromJust paymentStatus)
  whenJust paymentUrl $ QRB.updatePaymentUrl booking.id
  QRide.updateMultiple updRide.id updRide
  QFareBreakup.createMany breakups
  QPFS.clearCache booking.riderId
  createRecentLocationForTaxi booking
  checkAndUpdateJourneyTerminalStatusForNormalRide booking DJourney.COMPLETED

  -- uncomment for update api test; booking.paymentMethodId should be present
  -- whenJust booking.paymentMethodId $ \paymentMethodId -> do
  --   merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  --   paymentMethod <-
  --     CQMPM.findByIdAndMerchantId paymentMethodId booking.merchantId
  --       >>= fromMaybeM (MerchantPaymentMethodDoesNotExist paymentMethodId.getId)
  --   let dUpdateReq = ACL.PaymentCompletedBuildReq
  --         { bppBookingId,
  --           bppRideId = ride.bppRideId,
  --           paymentMethodInfo = DMPM.mkPaymentMethodInfo paymentMethod,
  --           bppId = booking.providerId,
  --           bppUrl = booking.providerUrl,
  --           transactionId = booking.transactionId,
  --           merchant
  --         }
  --   becknUpdateReq <- ACL.buildUpdateReq dUpdateReq
  --   void . withShortRetry $ CallBPP.update booking.providerUrl becknUpdateReq
  unless isInitiatedByCronJob $
    Notify.notifyOnRideCompleted booking updRide otherParties
  where
    buildFareBreakup :: MonadFlow m => Id DRide.Ride -> DFareBreakup -> m DFareBreakup.FareBreakup
    buildFareBreakup rideId DFareBreakup {..} = do
      guid <- generateGUID
      pure
        DFareBreakup.FareBreakup
          { id = guid,
            entityId = rideId.getId,
            entityType = DFareBreakup.RIDE,
            ..
          }

addOffersNammaTags ::
  ( MonadFlow m,
    CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasKafkaProducer r
  ) =>
  DRide.Ride ->
  DPerson.Person ->
  m ()
addOffersNammaTags ride person = do
  decryptedMobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (InternalError "Customer has no mobile number")
  now <- getCurrentTime
  let rideData = mkRideData ride
      customerData = Y.CustomerData {mobileNumber = decryptedMobileNumber, gender = person.gender}
  tags <- Yudhishthira.computeNammaTagsWithExpiry Yudhishthira.RideEndOffers (Y.EndRideOffersTagData customerData rideData)
  newTags <- modifiedNewNammaTags tags (fromMaybe [] person.customerNammaTags) now
  when (not $ null newTags) $ do
    QP.updateCustomerTags (Just $ (fromMaybe [] person.customerNammaTags) <> newTags) person.id
    pushToKafka (RideEndOffersKafkaData ride.id person.id newTags now) "customer-ride-end-offers" person.id.getId
    Notify.notifyOnRideEndOffer person
  where
    modifiedNewNammaTags newTags currTags now = do
      let currValidParsedTags =
            foldr
              ( \tag acc ->
                  case Yudhishthira.parseTag tag now of
                    Just (tagName, tagValue, validity) ->
                      case validity of
                        Just (Hours val) ->
                          if val > 0
                            then (tagName, tagValue, validity) : acc
                            else acc
                        Nothing -> (tagName, tagValue, validity) : acc
                    Nothing -> acc
              )
              []
              currTags
          newParsedTags = mapMaybe (\tag -> Yudhishthira.parseTag tag now) newTags
          newParsedTagsNotInCurrTags = filter (\(tagName, _, _) -> tagName `notElem` (currValidParsedTags <&> (\(tagName', _, _) -> tagName'))) newParsedTags
      modifiedParsedTags <-
        mapMaybeM
          ( \(LYT.TagName tagName, tagValue, validity) -> do
              case tagValue of
                LYT.TextValue tagValueText -> getOfferCodeModifiedTag tagName validity [tagValueText]
                LYT.ArrayValue tagValueTextArray -> getOfferCodeModifiedTag tagName validity tagValueTextArray
                _ -> pure Nothing
          )
          newParsedTagsNotInCurrTags
      return $
        map
          (\(tagName, tagValue, tagValidity) -> Yudhishthira.mkTagNameValueExpiry tagName tagValue tagValidity now)
          modifiedParsedTags

    getOfferCodeModifiedTag _ _ [] = pure Nothing
    getOfferCodeModifiedTag tagName validity tags@(tagValue : _) = do
      if tagValue == "Valid"
        then do
          mbOfferCode :: Maybe Text <- Redis.withCrossAppRedis $ Redis.rPop ("offerCodesPool-" <> tagName)
          case mbOfferCode of
            Just offerCode -> pure $ Just (LYT.TagName tagName, LYT.ArrayValue (tags <> [offerCode]), validity)
            Nothing -> pure Nothing
        else pure Nothing

    mkRideData DRide.Ride {updatedAt = updatedAt', ..} = Y.RideData {updatedAt = updatedAt', ..}

buildFareBreakupV2 :: MonadFlow m => Text -> DFareBreakup.FareBreakupEntityType -> DFareBreakup -> m DFareBreakup.FareBreakup
buildFareBreakupV2 entityId entityType DFareBreakup {..} = do
  guid <- generateGUID
  pure
    DFareBreakup.FareBreakup
      { id = guid,
        entityId,
        entityType,
        ..
      }

farePaidReqHandler :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => ValidatedFarePaidReq -> m ()
farePaidReqHandler req = void $ QRB.updatePaymentStatus req.booking.id req.paymentStatus

driverArrivedReqHandler ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    ServiceFlow m r
  ) =>
  ValidatedDriverArrivedReq ->
  m ()
driverArrivedReqHandler ValidatedDriverArrivedReq {..} = do
  unless (isJust ride.driverArrivalTime) $ do
    void $ notifyOnDriverArrived booking ride
    void $ QRide.updateDriverArrival ride.id arrivalTime
    QPFS.clearCache booking.riderId

bookingCancelledReqHandler ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    ClickhouseFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    m ~ Kernel.Types.Flow.FlowR AppEnv
  ) =>
  ValidatedBookingCancelledReq ->
  m ()
bookingCancelledReqHandler (ValidatedBookingCancelledReq {..}) = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason:-" <> show cancellationSource)
  cancellationTransaction booking mbRide cancellationSource Nothing

cancellationTransaction ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    ClickhouseFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    HasField "blackListedJobs" r [Text]
  ) =>
  DRB.Booking ->
  Maybe DRide.Ride ->
  DBCR.CancellationSource ->
  Maybe PriceAPIEntity ->
  m ()
cancellationTransaction booking mbRide cancellationSource cancellationFee = do
  bookingCancellationReason <- mkBookingCancellationReason booking (mbRide <&> (.id)) cancellationSource
  merchantConfigs <- CMC.findAllByMerchantOperatingCityIdInRideFlow booking.merchantOperatingCityId booking.configInExperimentVersions
  fork "incrementing fraud counters" $ do
    case mbRide of
      Just ride -> do
        case cancellationSource of
          DBCR.ByUser -> do
            handleUpgradedToCabRideCancellation
            SMC.updateCustomerFraudCounters booking.riderId merchantConfigs
          DBCR.ByDriver -> SMC.updateCancelledByDriverFraudCounters booking.riderId merchantConfigs
          _ -> pure ()
        triggerRideCancelledEvent RideEventData {ride = ride{status = DRide.CANCELLED}, personId = booking.riderId, merchantId = booking.merchantId}
      Nothing -> do
        logDebug "No ride found for the booking."
    let merchantOperatingCityId = booking.merchantOperatingCityId
    mFraudDetected <- SMC.anyFraudDetected booking.riderId merchantOperatingCityId merchantConfigs Nothing
    whenJust mFraudDetected $ \mc -> SMC.blockCustomer booking.riderId (Just mc.id)
  triggerBookingCancelledEvent BookingEventData {booking = booking{status = DRB.CANCELLED}}
  QPFS.updateStatus booking.riderId DPFS.IDLE
  otherParties <- Notify.getAllOtherRelatedPartyPersons booking
  unless (booking.status == DRB.CANCELLED) $
    void $ do
      QRB.updateStatus booking.id DRB.CANCELLED
      QBPL.makeAllInactiveByBookingId booking.id
      checkAndUpdateJourneyTerminalStatusForNormalRide booking DJourney.CANCELLED
  whenJust mbRide $ \ride -> void $ do
    void $ QRide.updateCancellationChargesOnCancel (maybe Nothing (Just . (.amount)) cancellationFee) ride.id
    unless (ride.status == DRide.CANCELLED) $ void $ QRide.updateStatus ride.id DRide.CANCELLED
  riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow booking.merchantOperatingCityId booking.configInExperimentVersions >>= fromMaybeM (InternalError "RiderConfig not found")
  fork "Cancellation Settlement" $ do
    whenJust cancellationFee $ \fee -> do
      case (riderConfig.settleCancellationFeeBeforeNextRide, mbRide) of
        (Just True, Just ride) -> do
          -- creating cancellation execution job which charges cancellation fee from users stripe account
          let scheduleAfter = riderConfig.cancellationPaymentDelay
              cancelExecutePaymentIntentJobData = CancelExecutePaymentIntentJobData {bookingId = booking.id, personId = booking.riderId, cancellationAmount = fee, rideId = ride.id}
          logDebug $ "Scheduling cancel execute payment intent job for order: " <> show scheduleAfter
          createJobIn @_ @'CancelExecutePaymentIntent (Just booking.merchantId) (Just booking.merchantOperatingCityId) scheduleAfter (cancelExecutePaymentIntentJobData :: CancelExecutePaymentIntentJobData)
        (_, Just ride) -> do
          when ride.onlinePayment $ do
            logInfo $ "Cancel payment intent due to rider configs: rideId: " <> ride.id.getId
            void $ SPayment.cancelPaymentIntent booking.merchantId booking.merchantOperatingCityId booking.paymentMode ride.id
        _ -> pure ()
    when (isNothing cancellationFee) $
      whenJust mbRide $ \ride -> do
        when ride.onlinePayment $ do
          logInfo $ "Cancel payment intent as no cancellation fees found: rideId: " <> ride.id.getId
          void $ SPayment.cancelPaymentIntent booking.merchantId booking.merchantOperatingCityId booking.paymentMode ride.id

  unless (cancellationSource == DBCR.ByUser) $
    QBCR.upsert bookingCancellationReason
  fork "Update namma tags and cancellation rate for customer cancellation" $ do
    when (cancellationSource == DBCR.ByUser) $ do
      case mbRide of
        Just ride -> do
          now <- getCurrentTime
          mbCallStatus <- QCallStatus.findOneByRideId (Just $ ride.id.getId)
          let callAtemptByDriver = isJust mbCallStatus
              currentTime = floor $ utcTimeToPOSIXSeconds now
              rideCreatedTime = floor $ utcTimeToPOSIXSeconds ride.createdAt
              driverArrivalTime = floor . utcTimeToPOSIXSeconds <$> ride.driverArrivalTime
              tagData =
                Y.CancelRideTagData
                  { ride = ride{status = DRide.CANCELLED},
                    booking = booking{status = DRB.CANCELLED},
                    cancellationReason = bookingCancellationReason,
                    callAtemptByDriver,
                    currentTime,
                    rideCreatedTime,
                    merchantOperatingCityId = booking.merchantOperatingCityId,
                    driverArrivalTime
                  }
          nammaTags <- withTryCatch "computeNammaTags:RideCancel" (Yudhishthira.computeNammaTags Yudhishthira.RideCancel tagData)
          logDebug $ "Tags for cancelled ride, rideId: " <> ride.id.getId <> " tagresults:" <> show (eitherToMaybe nammaTags)
          let mbNammaTags = eitherToMaybe nammaTags
          tagsWithExpiry <- forM (fromMaybe [] mbNammaTags) $ \tag -> Yudhishthira.fetchNammaTagExpiry tag
          person <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
          let existingTags = fromMaybe [] person.customerNammaTags
          let updatedTags = existingTags <> tagsWithExpiry
          QP.updateCustomerTags (Just updatedTags) booking.riderId
          let tags = fromMaybe [] mbNammaTags
          when (validCustomerCancellation `elem` tags) $ do
            windowSize <- CCR.getWindowSize booking.merchantOperatingCityId
            void $ CCR.incrementCancelledCount booking.riderId windowSize
            logDebug $ "Incremented cancellation count for customer: " <> booking.riderId.getId
        Nothing -> logError "No ride found for customer cancellation, skipping namma tags and cancellation rate update"
  fork "Checking lifetime blocking condition for customer based on cancellation rate" $ do
    val :: Maybe Bool <- Redis.safeGet $ makeCustomerBlockingKey booking.id.getId
    when (val == Just True) $ do
      Redis.del $ makeCustomerBlockingKey booking.id.getId
      personStats <- QPersonStats.findByPersonId booking.riderId
      case (personStats, riderConfig.minRidesToBlock, riderConfig.thresholdCancellationPercentageToBlock) of
        (Just stats, Just minRides, Just threshold) -> do
          let totalRides = stats.completedRides + stats.driverCancelledRides + stats.userCancelledRides
          let rate = (stats.userCancelledRides * 100) `div` max 1 totalRides
          when (totalRides > minRides && rate > threshold) $ do
            SMC.blockCustomer booking.riderId Nothing
        _ -> logDebug "Configs or person stats doesnt not exist for checking blocking condition"
  -- notify customer
  bppDetails <- CQBPP.findBySubscriberIdAndDomain booking.providerId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> booking.providerId <> "and domain:-" <> show Context.MOBILITY)
  Notify.notifyOnBookingCancelled booking cancellationSource bppDetails mbRide otherParties
  when (booking.isDashboardRequest == Just True && riderConfig.autoSendBookingDetailsViaWhatsapp == Just True) $ do
    fork "Sending Dashboard Cancelled Ride Message" $ do
      sendBookingCancelledMessageViaWhatsapp booking.riderId riderConfig
  where
    handleUpgradedToCabRideCancellation =
      case booking.bookingDetails of
        DRB.OneWayDetails details ->
          when (details.isUpgradedToCab == Just True) $ do
            person <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
            let personTags = fromMaybe [] person.customerNammaTags
            unless (rejectUpgradeTag `Yudhishthira.elemTagNameValue` personTags) $ do
              rejectUpgradeTagWithExpiry <- Yudhishthira.fetchNammaTagExpiry rejectUpgradeTag
              QP.updateCustomerTags (Just $ personTags <> [rejectUpgradeTagWithExpiry]) person.id
        _ -> pure ()

checkAndUpdateJourneyTerminalStatusForNormalRide :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => DRB.Booking -> DJourney.JourneyStatus -> m ()
checkAndUpdateJourneyTerminalStatusForNormalRide booking journeyStatus = do
  mbJourneyId <- getJourneyIdFromBooking booking
  whenJust mbJourneyId $ \journeyId -> do
    journeyLegs <- QJL.getJourneyLegs journeyId
    case journeyLegs of
      [_] -> QJourney.updateStatus journeyStatus journeyId -- only one element here means just taxi leg i.e. normal ride flow, so updating journeyStatus
      _ -> pure ()

mkBookingCancellationReason ::
  (MonadFlow m) =>
  DRB.Booking ->
  Maybe (Id DRide.Ride) ->
  DBCR.CancellationSource ->
  m DBCR.BookingCancellationReason
mkBookingCancellationReason booking mbRideId cancellationSource = do
  now <- getCurrentTime
  return $
    DBCR.BookingCancellationReason
      { bookingId = booking.id,
        merchantId = Just booking.merchantId,
        distanceUnit = booking.distanceUnit,
        rideId = mbRideId,
        source = cancellationSource,
        reasonCode = Nothing,
        reasonStage = Nothing,
        additionalInfo = Nothing,
        driverCancellationLocation = Nothing,
        driverDistToPickup = Nothing,
        riderId = Just booking.riderId,
        createdAt = now,
        updatedAt = now
      }

validateRideAssignedReq ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance)
  ) =>
  RideAssignedReq ->
  m ValidatedRideAssignedReq
validateRideAssignedReq RideAssignedReq {..} = do
  booking <- QRB.findByTransactionId transactionId >>= fromMaybeM (BookingDoesNotExist $ "transactionId:-" <> transactionId)
  mbMerchant <- CQM.findById booking.merchantId
  let onlinePayment = SPayment.isOnlinePayment mbMerchant booking
  -- TODO: Should we put 'TRIP_ASSIGNED' status check in the 'isAssignable' function for normal booking Or Handle for crone Job in Different Way?
  unless (isAssignable booking) $ throwError (BookingInvalidStatus $ show booking.status)
  onlinePaymentParameters <-
    if onlinePayment
      then do
        person <- runInReplica $ QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
        (customerPaymentId, paymentMethodId) <- SPayment.getCustomerAndPaymentMethod booking person
        driverAccountId_ <- driverAccountId & fromMaybeM (DriverAccountIdNotFound booking.id.getId)
        let merchantOperatingCityId = person.merchantOperatingCityId
        email <- mapM decrypt person.email
        return $ Just OnlinePaymentParameters {driverAccountId = driverAccountId_, ..}
      else return Nothing
  return $ ValidatedRideAssignedReq {..}
  where
    isAssignable booking = booking.status `elem` (if booking.isScheduled then [DRB.CONFIRMED, DRB.AWAITING_REASSIGNMENT, DRB.NEW, DRB.TRIP_ASSIGNED] else [DRB.CONFIRMED, DRB.AWAITING_REASSIGNMENT, DRB.NEW])

validateRideStartedReq ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance)
  ) =>
  RideStartedReq ->
  m ValidatedRideStartedReq
validateRideStartedReq RideStartedReq {..} = do
  let BookingDetails {..} = bookingDetails
  (ride, booking) <- getRideAndBooking bppBookingId transactionId
  unless (booking.status == DRB.TRIP_ASSIGNED) $ throwError (BookingInvalidStatus $ show booking.status)
  unless (ride.status == DRide.NEW || ride.status == DRide.UPCOMING) $ throwError (RideInvalidStatus $ show ride.status)
  let estimatedEndTimeRange = mkEstimatedEndTimeRange <$> estimatedEndTimeRangeStart <*> estimatedEndTimeRangeEnd
  return $ ValidatedRideStartedReq {..}
  where
    mkEstimatedEndTimeRange start end = DRide.EstimatedEndTimeRange {start, end}

validateDriverArrivedReq ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance)
  ) =>
  DriverArrivedReq ->
  m ValidatedDriverArrivedReq
validateDriverArrivedReq DriverArrivedReq {..} = do
  let BookingDetails {..} = bookingDetails
  (ride, booking) <- getRideAndBooking bppBookingId transactionId
  unless (isValidRideStatus ride.status) $ throwError $ RideInvalidStatus ("The ride has already started." <> Text.pack (show ride.status))
  return $ ValidatedDriverArrivedReq {..}
  where
    isValidRideStatus status = status == DRide.NEW

validateRideCompletedReq ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance)
  ) =>
  RideCompletedReq ->
  m (Either ValidatedRideCompletedReq ValidatedFarePaidReq)
validateRideCompletedReq RideCompletedReq {..} = do
  let BookingDetails {..} = bookingDetails
  (ride, booking) <- getRideAndBooking bppBookingId transactionId
  let bookingCanBeCompleted = booking.status == DRB.TRIP_ASSIGNED
      rideIsNew = ride.status == DRide.NEW
      rideCanBeCompleted = ride.status == DRide.INPROGRESS
      bookingAlreadyCompleted = booking.status == DRB.COMPLETED
      rideAlreadyCompleted = ride.status == DRide.COMPLETED
  if bookingAlreadyCompleted && rideAlreadyCompleted
    then validateFarePaidReq booking
    else do
      unless (isInitiatedByCronJob || bookingCanBeCompleted || (bookingAlreadyCompleted && rideCanBeCompleted)) $
        throwError (BookingInvalidStatus $ show booking.status)
      unless (isInitiatedByCronJob || rideCanBeCompleted || ((rideAlreadyCompleted || rideIsNew) && bookingCanBeCompleted)) $
        throwError (RideInvalidStatus $ show ride.status)
      person <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
      return . Left $ ValidatedRideCompletedReq {..}
  where
    validateFarePaidReq booking = do
      when (booking.paymentStatus == Just DRB.PAID) $ do
        throwError . InvalidRequest $ "payment_status is already PAID for bookingId:-" <> show booking.id.getId
      when (paymentStatus /= Just DRB.PAID) $ do
        throwError . InvalidRequest $ "Invalid payment status change:-" <> show paymentStatus <> " for bookingId:-" <> show booking.id.getId <> ", which is already completed."
      return . Right $ ValidatedFarePaidReq {booking, paymentStatus = fromJust paymentStatus} -- fromJust is safe here because of above check.

validateBookingCancelledReq ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance)
  ) =>
  BookingCancelledReq ->
  m ValidatedBookingCancelledReq
validateBookingCancelledReq BookingCancelledReq {..} = do
  let isInitiatedByCronJob = maybe False (.isInitiatedByCronJob) bookingDetails
  mBooking <- QRB.findByBPPBookingId bppBookingId
  booking <- case mBooking of
    Nothing -> do
      logInfo $ "Booking not found for bppBookingId: " <> bppBookingId.getId
      QRB.findByTransactionId transactionId >>= fromMaybeM (BookingDoesNotExist $ "TransactionId: " <> transactionId)
    Just booking -> return booking
  mbRide <- QRide.findActiveByRBId booking.id
  let isRideCancellable = maybe False (\ride -> ride.status `notElem` [DRide.INPROGRESS, DRide.CANCELLED]) mbRide
      bookingAlreadyCancelled = booking.status == DRB.CANCELLED
  unless (isInitiatedByCronJob || isBookingCancellable booking || (isRideCancellable && bookingAlreadyCancelled)) $
    throwError (BookingInvalidStatus (show booking.status))
  return $ ValidatedBookingCancelledReq {..}
  where
    isBookingCancellable booking =
      booking.status `elem` [DRB.NEW, DRB.CONFIRMED, DRB.AWAITING_REASSIGNMENT, DRB.TRIP_ASSIGNED]

buildPersonClientInfo :: MonadFlow m => Id DPerson.Person -> Maybe (Id DC.Client) -> Id DMOC.MerchantOperatingCity -> Id DMerchant.Merchant -> BecknEnums.VehicleCategory -> Int -> m DPCI.ClientPersonInfo
buildPersonClientInfo personId clientId cityId merchantId vehicleCategory rideCount = do
  now <- getCurrentTime
  id <- generateGUID
  return
    DPCI.ClientPersonInfo
      { id = id,
        personId = personId,
        clientId = clientId,
        merchantOperatingCityId = cityId,
        merchantId = merchantId,
        vehicleCategory = Just vehicleCategory,
        rideCount = rideCount,
        createdAt = now,
        updatedAt = now
      }

sendRideEndMessage ::
  ( HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    HasKafkaProducer r
  ) =>
  DRB.Booking ->
  m ()
sendRideEndMessage bk = case bk.tripCategory of
  Just (Trip.Delivery _) -> do
    senderParty <- QBPL.findOneActiveByBookingIdAndTripParty bk.id (Trip.DeliveryParty Trip.Sender) >>= fromMaybeM (InternalError $ "Sender booking party not found for " <> bk.id.getId)
    receiverParty <- QBPL.findOneActiveByBookingIdAndTripParty bk.id (Trip.DeliveryParty Trip.Receiver) >>= fromMaybeM (InternalError $ "Receiver booking party not found for " <> bk.id.getId)
    senderPerson <- QP.findById senderParty.partyId >>= fromMaybeM (PersonDoesNotExist senderParty.partyId.getId)
    senderMobileNumber <- mapM decrypt senderPerson.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
    fork "Sending end delivery message to sender" $ do
      let countryCode = fromMaybe "+91" senderPerson.mobileCountryCode
          phoneNumber = countryCode <> senderMobileNumber
          senderSmsReq =
            MessageBuilder.BuildDeliveryMessageReq
              { MessageBuilder.driverName = "",
                MessageBuilder.driverNumber = "",
                MessageBuilder.trackingUrl = Nothing,
                MessageBuilder.appUrl = "",
                MessageBuilder.senderName = senderParty.partyName,
                MessageBuilder.receiverName = receiverParty.partyName,
                MessageBuilder.otp = "",
                MessageBuilder.hasEnded = True,
                MessageBuilder.pickedUp = True,
                MessageBuilder.deliveryMessageType = MessageBuilder.SenderReq
              }
      buildSmsReq <- MessageBuilder.buildDeliveryDetailsMessage bk.merchantOperatingCityId senderSmsReq
      Sms.sendSMS bk.merchantId bk.merchantOperatingCityId (buildSmsReq phoneNumber) >>= Sms.checkSmsResult
  _ -> pure ()

customerReferralPayout ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r
  ) =>
  DRide.Ride ->
  Maybe Bool ->
  DRC.RiderConfig ->
  DPerson.Person ->
  Id DMerchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
customerReferralPayout ride isValidRide riderConfig person_ merchantId merchantOperatingCityId = do
  let vehicleCategory = DV.castVehicleVariantToVehicleCategory ride.vehicleVariant
  logDebug $ "Ride End referral payout : vehicleCategory : " <> show vehicleCategory <> " isValidRide: " <> show isValidRide
  mbPayoutConfig <- CPC.findByCityIdAndVehicleCategory merchantOperatingCityId vehicleCategory Nothing
  case mbPayoutConfig of
    Just payoutConfig -> do
      whenJust person_.referredByCustomer $ \referredByCustomerId -> do
        referredByPersonStats <- getPersonStats (Id referredByCustomerId)
        personsWithSameDeviceId <- QP.findAllByDeviceId person_.deviceId
        let dailyPayoutCountKey = getDailyPayoutCountKey referredByCustomerId
            monthlyPayoutCountKey = getMonthlyPayoutKey referredByCustomerId
        dailyPayoutCount_ <- Redis.get dailyPayoutCountKey
        monthlyPayoutCount <- fromIntegral <$> SWC.getCurrentWindowCount monthlyPayoutCountKey SW.SlidingWindowOptions {period = 1, periodType = SW.Months}
        let dailyPayoutCount = fromMaybe 0 dailyPayoutCount_ -- for referredBy customer
            isDeviceIdValid = length personsWithSameDeviceId == 1 -- deviceId of new customer should be unique
            deviceIdCheck = fromMaybe False riderConfig.isDeviceIdCheckDisabled || isDeviceIdValid
            isConsideredForPayout = maybe False (\referredAt -> referredAt >= riderConfig.payoutReferralStartDate) person_.referredAt
            payoutProgramThresholdChecks = (dailyPayoutCount < riderConfig.payoutReferralThresholdPerDay) && (monthlyPayoutCount < riderConfig.payoutReferralThresholdPerMonth) && deviceIdCheck
        when (isConsideredForPayout && payoutProgramThresholdChecks && fromMaybe False isValidRide && riderConfig.payoutReferralProgram && payoutConfig.isPayoutEnabled) $ do
          personStats <- getPersonStats person_.id
          let shouldProcessRefereePayout = payoutConfig.referredByRewardAmount > 0.0
              shouldProcessReferrerPayout = payoutConfig.referralRewardAmountPerRide > 0.0
          when shouldProcessRefereePayout $ do
            QPersonStats.updateReferredByEarning (personStats.referredByEarnings + payoutConfig.referredByRewardAmount) person_.id
            sendPNToPerson person_ False
            handlePayout person_ payoutConfig.referredByRewardAmount payoutConfig False referredByPersonStats DLP.REFERRED_BY_AWARD dailyPayoutCount
          when shouldProcessReferrerPayout $ do
            referredByPerson <- QP.findById (Id referredByCustomerId) >>= fromMaybeM (PersonNotFound referredByCustomerId)
            sendPNToPerson referredByPerson True
            handlePayout referredByPerson payoutConfig.referralRewardAmountPerRide payoutConfig True referredByPersonStats DLP.REFERRAL_AWARD_RIDE dailyPayoutCount
    Nothing -> logTagError "Payout Config Error" $ "PayoutConfig Not Found for cityId: " <> merchantOperatingCityId.getId <> " and category: " <> show vehicleCategory
  where
    handlePayout person amount payoutConfig isReferredByPerson referredByPersonStats entity dailyPayoutCount = do
      case person.payoutVpa of
        Just vpa -> do
          Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey person.id.getId) 5 5 $ do
            case isReferredByPerson of
              True -> do
                QPersonStats.updateReferralEarningsAndValidActivations (referredByPersonStats.referralEarnings + payoutConfig.referralRewardAmountPerRide) (referredByPersonStats.validActivations + 1) person.id
                setPayoutCountForReferree person.id.getId dailyPayoutCount
              False -> QPersonStats.updateReferredByEarningsPayoutStatus (Just DPS.Processing) person.id
            phoneNo <- mapM decrypt person.mobileNumber
            emailId <- mapM decrypt person.email
            uid <- generateGUID
            let entityName = entity
                createPayoutOrderReq = Payout.mkCreatePayoutOrderReq uid amount phoneNo emailId person.id.getId payoutConfig.remark person.firstName vpa payoutConfig.orderType True
            logDebug $ "create payoutOrder with riderId: " <> person.id.getId <> " | amount: " <> show amount <> " | orderId: " <> show uid
            payoutServiceName <- TP.decidePayoutService (DEMSC.PayoutService PT.Juspay) person.clientSdkVersion
            let createPayoutOrderCall = TP.createPayoutOrder merchantId merchantOperatingCityId payoutServiceName (Just person.id.getId)
            merchantOperatingCity <- CQMOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
            mbPayoutOrderResp <- withTryCatch "createPayoutService:handlePayout" $ Payout.createPayoutService (cast merchantId) (Just $ cast merchantOperatingCityId) (cast person.id) (Just [ride.id.getId]) (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall
            case mbPayoutOrderResp of
              Left err -> logError $ "Error in calling create payout rideId: " <> show ride.id.getId <> " and orderId: " <> show uid <> "with error " <> show err
              _ -> pure ()
        Nothing -> do
          when isReferredByPerson $ do
            setPayoutCountForReferree person.id.getId dailyPayoutCount
            QPersonStats.updateEarningsAndActivations (referredByPersonStats.referralEarnings + amount) (referredByPersonStats.backlogPayoutAmount + amount) (referredByPersonStats.validActivations + 1) person.id

    getPersonStats personId = do
      mbPersonStats <- QPersonStats.findByPersonId personId
      case mbPersonStats of
        Just personStats -> pure personStats
        Nothing -> do
          pStats <- mkPersonStats personId
          QPersonStats.create pStats
          pure pStats

    mkPersonStats personId = do
      now <- getCurrentTime
      return
        DPS.PersonStats
          { personId = personId,
            userCancelledRides = 0,
            driverCancelledRides = 0,
            completedRides = 0,
            weekendRides = 0,
            weekdayRides = 0,
            offPeakRides = 0,
            eveningPeakRides = 0,
            morningPeakRides = 0,
            weekendPeakRides = 0,
            referralCount = 0,
            createdAt = now,
            updatedAt = now,
            ticketsBookedInEvent = Just 0,
            referralAmountPaid = 0,
            referralEarnings = 0,
            referredByEarnings = 0,
            validActivations = 0,
            referredByEarningsPayoutStatus = Nothing,
            backlogPayoutStatus = Nothing,
            backlogPayoutAmount = 0,
            isBackfilled = Just False
          }

    getExpirationSeconds timeDiffFromUtc = do
      currentUtcTime <- getCurrentTime
      let localTime = addUTCTime (KUT.secondsToNominalDiffTime timeDiffFromUtc) currentUtcTime
          nextLocalMidnight = UTCTime (addDays 1 (utctDay localTime)) 0
          secondsUntilExpiration = round $ diffUTCTime nextLocalMidnight localTime
      pure secondsUntilExpiration

    setPayoutCountForReferree personId dailyPayoutCount = do
      expirationPeriodForDay <- getExpirationSeconds riderConfig.timeDiffFromUtc
      let dailyPayoutCountKey = getDailyPayoutCountKey personId
          monthlyPayoutCountKey = getMonthlyPayoutKey personId
      Redis.setExp dailyPayoutCountKey (dailyPayoutCount + 1) expirationPeriodForDay
      SWC.incrementByValue 1 monthlyPayoutCountKey SW.SlidingWindowOptions {period = 1, periodType = SW.Months}

    sendPNToPerson person oldCustomer =
      when (isNothing person.payoutVpa) $ do
        let pnKey =
              if oldCustomer
                then "REFERRAL_REWARD_ADD_VPA"
                else "REFERRED_BY_REWARD_ADD_VPA"
        mbMerchantPN <- CPN.findMatchingMerchantPNInRideFlow merchantOperatingCityId pnKey Nothing Nothing person.language []
        whenJust mbMerchantPN $ \merchantPN -> do
          let entityData = Notify.NotifReq {title = merchantPN.title, message = merchantPN.body}
          Notify.notifyPersonOnEvents person entityData merchantPN.fcmNotificationType

payoutProcessingLockKey :: Text -> Text
payoutProcessingLockKey personId = "Payout:Processing:PersonId" <> personId

getDailyPayoutCountKey :: Text -> Text
getDailyPayoutCountKey personId = "Payout:Daily:PId" <> personId

getMonthlyPayoutKey :: Text -> Text
getMonthlyPayoutKey personId = "Payout:Monthly:PId-" <> personId

sendRideBookingDetailsViaWhatsapp ::
  ( CacheFlow m r,
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    HasKafkaProducer r
  ) =>
  Id DPerson.Person ->
  DRide.Ride ->
  DRB.Booking ->
  DRC.RiderConfig ->
  m ()
sendRideBookingDetailsViaWhatsapp personId ride booking riderConfig = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  bookings <- QRB.findAllByTransactionId booking.transactionId
  let phoneNumber = countryCode <> mobileNumber
      trackLink = Notify.buildTrackingUrl ride.id [("vp", "shareRide")] riderConfig.trackingShortUrlPattern
      messageKey = bool DMM.WHATSAPP_CALL_BOOKING_FLOW_DETAILS_MESSAGE DMM.WHATSAPP_CALL_BOOKING_REALLOCATED_RIDE_DETAILS_MESSAGE (length bookings > 1)
  _ <- MessageBuilder.shortenTrackingUrl trackLink
  merchantMessage <- CMM.findByMerchantOperatingCityIdAndMessageKeyInRideFlow person.merchantOperatingCityId messageKey booking.configInExperimentVersions >>= fromMaybeM (MerchantMessageNotFound person.merchantOperatingCityId.getId (show messageKey))
  let driverNumber = (fromMaybe "+91" ride.driverMobileCountryCode) <> ride.driverMobileNumber
      fare = show booking.estimatedTotalFare.amount
  result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI person.merchantId person.merchantOperatingCityId (Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq phoneNumber merchantMessage.templateId [Just driverNumber, Just ride.vehicleNumber, Just fare, Just ride.otp, Just "N/A", Just riderConfig.appUrl] Nothing Nothing) -- Accepts at most 7 variables using GupShup
  when (result._response.status /= "success") $ throwError (InternalError "Unable to send Dashboard Ride Booking Details Whatsapp message")

sendBookingCancelledMessageViaWhatsapp ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    HasKafkaProducer r
  ) =>
  Id DPerson.Person ->
  DRC.RiderConfig ->
  m ()
sendBookingCancelledMessageViaWhatsapp personId riderConfig = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  let phoneNumber = countryCode <> mobileNumber
      messageKey = DMM.WHATSAPP_CALL_BOOKING_CANCELLED_RIDE_MESSAGE
  merchantMessage <- CMM.findByMerchantOperatingCityIdAndMessageKey person.merchantOperatingCityId messageKey Nothing >>= fromMaybeM (MerchantMessageNotFound person.merchantOperatingCityId.getId (show messageKey))
  result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI person.merchantId person.merchantOperatingCityId (Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq phoneNumber merchantMessage.templateId [Just riderConfig.appUrl] Nothing Nothing) -- Accepts at most 7 variables using GupShup
  when (result._response.status /= "success") $ throwError (InternalError "Unable to send Dashboard Cancelled Booking Whatsapp message")

notifyOnDriverArrived :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, MonadFlow m, ServiceFlow m r) => DRB.Booking -> DRide.Ride -> m ()
notifyOnDriverArrived booking ride = do
  mbHasReachedNotified <- Redis.safeGet @() driverHasReached
  when (isNothing mbHasReachedNotified) $ do
    Notify.notifyDriverHasReached booking.riderId booking.tripCategory ride.otp ride.vehicleNumber ride.vehicleColor ride.vehicleModel ride.vehicleVariant
    Redis.setExp driverHasReached () 1500
  where
    driverHasReached = driverHasReachedCacheKey ride.id.getId

driverHasReachedCacheKey :: Text -> Text
driverHasReachedCacheKey rideId = "Ride:GetDriverLoc:DriverHasReached " <> rideId

createRecentLocationForTaxi :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DRB.Booking -> m ()
createRecentLocationForTaxi booking = do
  now <- getCurrentTime
  let mbToLocation = case booking.bookingDetails of
        DRB.OneWayDetails details -> Just details.toLocation
        DRB.RentalDetails _ -> Nothing
        DRB.DriverOfferDetails details -> Just details.toLocation
        DRB.OneWaySpecialZoneDetails details -> Just details.toLocation
        DRB.InterCityDetails details -> Just details.toLocation
        DRB.AmbulanceDetails details -> Just details.toLocation
        DRB.DeliveryDetails details -> Just details.toLocation
        DRB.MeterRideDetails details -> details.toLocation

  whenJust mbToLocation $ \toLocation -> do
    let address' =
          Text.intercalate ", " $
            catMaybes
              [ toLocation.address.title,
                toLocation.address.building,
                toLocation.address.street,
                toLocation.address.city,
                toLocation.address.state,
                toLocation.address.country
              ]
        -- Generate geohash with precision of ~100 meters (precision level 6)
        toGeohash = Text.pack <$> Geohash.encode 6 (toLocation.lat, toLocation.lon)
        fromGeohash = Text.pack <$> Geohash.encode 6 (booking.fromLocation.lat, booking.fromLocation.lon)

    -- Search for existing recent location with same geohash and entity type
    mbExistingLocation <- SQRL.findByRiderIdAndGeohashAndEntityType booking.riderId toGeohash fromGeohash DTRL.TAXI
    case mbExistingLocation of
      Just existingLocation -> do
        -- If found, increase frequency
        SQRL.increaceFrequencyById existingLocation.id
      Nothing -> do
        -- If not found, create new recent location
        uuid <- generateGUID
        let recentLocation =
              DTRL.RecentLocation
                { DTRL.address = Just address',
                  DTRL.entityType = DTRL.TAXI,
                  DTRL.frequency = 1,
                  DTRL.fromStopCode = Nothing,
                  DTRL.id = uuid,
                  DTRL.toLatLong = LatLong toLocation.lat toLocation.lon,
                  DTRL.merchantOperatingCityId = booking.merchantOperatingCityId,
                  DTRL.riderId = booking.riderId,
                  DTRL.routeCode = Nothing,
                  DTRL.toStopCode = Nothing,
                  DTRL.fromLatLong = Just $ LatLong booking.fromLocation.lat booking.fromLocation.lon,
                  DTRL.createdAt = now,
                  DTRL.updatedAt = now,
                  DTRL.fare = Just booking.estimatedTotalFare.amount,
                  DTRL.toGeohash = toGeohash,
                  DTRL.fromGeohash = fromGeohash
                }
        SQRL.create recentLocation

getRideAndBooking :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id DRB.BPPBooking -> Text -> m (DRide.Ride, DRB.Booking)
getRideAndBooking bppBookingId transactionId = do
  mBooking <- QRB.findByBPPBookingId bppBookingId
  booking <- case mBooking of
    Nothing -> do
      logInfo $ "Booking not found for bppBookingId: " <> bppBookingId.getId
      QRB.findByTransactionId transactionId >>= fromMaybeM (BookingDoesNotExist $ "TransactionId: " <> transactionId)
    Just booking -> return booking
  ride <- QRide.findByRBId booking.id >>= fromMaybeM (RideDoesNotExist $ "bookingId: " <> bppBookingId.getId)
  return (ride, booking)
