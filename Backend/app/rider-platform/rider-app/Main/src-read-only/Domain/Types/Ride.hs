{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Ride where

import Data.Aeson
import qualified Domain.Types.Booking
import qualified Domain.Types.Client
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.VehicleVariant
import Kernel.External.Encryption
import qualified Kernel.External.Maps
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Confidence
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import qualified Kernel.Types.Version
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data RideE e = Ride
  { allowedEditLocationAttempts :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    allowedEditPickupLocationAttempts :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    bppRideId :: Kernel.Types.Id.Id Domain.Types.Ride.BPPRide,
    cancellationFeeIfCancelled :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    chargeableDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    clientId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Client.Client),
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    createdAt :: Kernel.Prelude.UTCTime,
    destinationReachedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverAccountId :: Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.AccountId,
    driverAlternateNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    driverArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverMobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverMobileNumber :: Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    driverPhoneNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    driverRating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    driverRegisteredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driversPreviousRideDropLoc :: Kernel.Prelude.Maybe Kernel.External.Maps.LatLong,
    endOdometerReading :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    endOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    estimatedEndTimeRange :: Kernel.Prelude.Maybe Domain.Types.Ride.EstimatedEndTimeRange,
    fare :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    favCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    feedbackSkipped :: Kernel.Prelude.Bool,
    fromLocation :: Domain.Types.Location.Location,
    hasStops :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    isAlreadyFav :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isFreeRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    onlinePayment :: Kernel.Prelude.Bool,
    otp :: Kernel.Prelude.Text,
    paymentStatus :: Domain.Types.Ride.PaymentStatus,
    pickupRouteCallCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rideEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideRating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rideStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    safetyCheckStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    safetyJourneyStatus :: Kernel.Prelude.Maybe Domain.Types.Ride.SosJourneyStatus,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.Ride.Ride,
    showDriversPreviousRideDropLoc :: Kernel.Prelude.Bool,
    startOdometerReading :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    status :: Domain.Types.Ride.RideStatus,
    stops :: [Domain.Types.Location.Location],
    talkedWithDriver :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    tipAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    tollConfidence :: Kernel.Prelude.Maybe Kernel.Types.Confidence.Confidence,
    totalFare :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    trackingUrl :: Kernel.Prelude.Maybe Kernel.Types.Common.BaseUrl,
    traveledDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleAge :: Kernel.Prelude.Maybe Kernel.Types.Time.Months,
    vehicleColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text,
    vehicleServiceTierType :: Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType,
    vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant,
    wasRideSafe :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving (Generic)

type Ride = RideE 'AsEncrypted

type DecryptedRide = RideE 'AsUnencrypted

instance EncryptedItem Ride where
  type Unencrypted Ride = (DecryptedRide, HashSalt)
  encryptItem (entity, salt) = do
    driverAlternateNumber_ <- encryptItem $ (,salt) <$> driverAlternateNumber entity
    driverPhoneNumber_ <- encryptItem $ (,salt) <$> driverPhoneNumber entity
    pure
      Ride
        { allowedEditLocationAttempts = allowedEditLocationAttempts entity,
          allowedEditPickupLocationAttempts = allowedEditPickupLocationAttempts entity,
          backendAppVersion = backendAppVersion entity,
          backendConfigVersion = backendConfigVersion entity,
          bookingId = bookingId entity,
          bppRideId = bppRideId entity,
          cancellationFeeIfCancelled = cancellationFeeIfCancelled entity,
          chargeableDistance = chargeableDistance entity,
          clientBundleVersion = clientBundleVersion entity,
          clientConfigVersion = clientConfigVersion entity,
          clientDevice = clientDevice entity,
          clientId = clientId entity,
          clientSdkVersion = clientSdkVersion entity,
          createdAt = createdAt entity,
          destinationReachedAt = destinationReachedAt entity,
          distanceUnit = distanceUnit entity,
          driverAccountId = driverAccountId entity,
          driverAlternateNumber = driverAlternateNumber_,
          driverArrivalTime = driverArrivalTime entity,
          driverImage = driverImage entity,
          driverMobileCountryCode = driverMobileCountryCode entity,
          driverMobileNumber = driverMobileNumber entity,
          driverName = driverName entity,
          driverPhoneNumber = driverPhoneNumber_,
          driverRating = driverRating entity,
          driverRegisteredAt = driverRegisteredAt entity,
          driversPreviousRideDropLoc = driversPreviousRideDropLoc entity,
          endOdometerReading = endOdometerReading entity,
          endOtp = endOtp entity,
          estimatedEndTimeRange = estimatedEndTimeRange entity,
          fare = fare entity,
          favCount = favCount entity,
          feedbackSkipped = feedbackSkipped entity,
          fromLocation = fromLocation entity,
          hasStops = hasStops entity,
          id = id entity,
          isAlreadyFav = isAlreadyFav entity,
          isFreeRide = isFreeRide entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          onlinePayment = onlinePayment entity,
          otp = otp entity,
          paymentStatus = paymentStatus entity,
          pickupRouteCallCount = pickupRouteCallCount entity,
          rideEndTime = rideEndTime entity,
          rideRating = rideRating entity,
          rideStartTime = rideStartTime entity,
          safetyCheckStatus = safetyCheckStatus entity,
          safetyJourneyStatus = safetyJourneyStatus entity,
          shortId = shortId entity,
          showDriversPreviousRideDropLoc = showDriversPreviousRideDropLoc entity,
          startOdometerReading = startOdometerReading entity,
          status = status entity,
          stops = stops entity,
          talkedWithDriver = talkedWithDriver entity,
          tipAmount = tipAmount entity,
          toLocation = toLocation entity,
          tollConfidence = tollConfidence entity,
          totalFare = totalFare entity,
          trackingUrl = trackingUrl entity,
          traveledDistance = traveledDistance entity,
          updatedAt = updatedAt entity,
          vehicleAge = vehicleAge entity,
          vehicleColor = vehicleColor entity,
          vehicleModel = vehicleModel entity,
          vehicleNumber = vehicleNumber entity,
          vehicleServiceTierType = vehicleServiceTierType entity,
          vehicleVariant = vehicleVariant entity,
          wasRideSafe = wasRideSafe entity
        }
  decryptItem entity = do
    driverAlternateNumber_ <- fmap fst <$> decryptItem (driverAlternateNumber entity)
    driverPhoneNumber_ <- fmap fst <$> decryptItem (driverPhoneNumber entity)
    pure
      ( Ride
          { allowedEditLocationAttempts = allowedEditLocationAttempts entity,
            allowedEditPickupLocationAttempts = allowedEditPickupLocationAttempts entity,
            backendAppVersion = backendAppVersion entity,
            backendConfigVersion = backendConfigVersion entity,
            bookingId = bookingId entity,
            bppRideId = bppRideId entity,
            cancellationFeeIfCancelled = cancellationFeeIfCancelled entity,
            chargeableDistance = chargeableDistance entity,
            clientBundleVersion = clientBundleVersion entity,
            clientConfigVersion = clientConfigVersion entity,
            clientDevice = clientDevice entity,
            clientId = clientId entity,
            clientSdkVersion = clientSdkVersion entity,
            createdAt = createdAt entity,
            destinationReachedAt = destinationReachedAt entity,
            distanceUnit = distanceUnit entity,
            driverAccountId = driverAccountId entity,
            driverAlternateNumber = driverAlternateNumber_,
            driverArrivalTime = driverArrivalTime entity,
            driverImage = driverImage entity,
            driverMobileCountryCode = driverMobileCountryCode entity,
            driverMobileNumber = driverMobileNumber entity,
            driverName = driverName entity,
            driverPhoneNumber = driverPhoneNumber_,
            driverRating = driverRating entity,
            driverRegisteredAt = driverRegisteredAt entity,
            driversPreviousRideDropLoc = driversPreviousRideDropLoc entity,
            endOdometerReading = endOdometerReading entity,
            endOtp = endOtp entity,
            estimatedEndTimeRange = estimatedEndTimeRange entity,
            fare = fare entity,
            favCount = favCount entity,
            feedbackSkipped = feedbackSkipped entity,
            fromLocation = fromLocation entity,
            hasStops = hasStops entity,
            id = id entity,
            isAlreadyFav = isAlreadyFav entity,
            isFreeRide = isFreeRide entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            onlinePayment = onlinePayment entity,
            otp = otp entity,
            paymentStatus = paymentStatus entity,
            pickupRouteCallCount = pickupRouteCallCount entity,
            rideEndTime = rideEndTime entity,
            rideRating = rideRating entity,
            rideStartTime = rideStartTime entity,
            safetyCheckStatus = safetyCheckStatus entity,
            safetyJourneyStatus = safetyJourneyStatus entity,
            shortId = shortId entity,
            showDriversPreviousRideDropLoc = showDriversPreviousRideDropLoc entity,
            startOdometerReading = startOdometerReading entity,
            status = status entity,
            stops = stops entity,
            talkedWithDriver = talkedWithDriver entity,
            tipAmount = tipAmount entity,
            toLocation = toLocation entity,
            tollConfidence = tollConfidence entity,
            totalFare = totalFare entity,
            trackingUrl = trackingUrl entity,
            traveledDistance = traveledDistance entity,
            updatedAt = updatedAt entity,
            vehicleAge = vehicleAge entity,
            vehicleColor = vehicleColor entity,
            vehicleModel = vehicleModel entity,
            vehicleNumber = vehicleNumber entity,
            vehicleServiceTierType = vehicleServiceTierType entity,
            vehicleVariant = vehicleVariant entity,
            wasRideSafe = wasRideSafe entity
          },
        ""
      )

instance EncryptedItem' Ride where
  type UnencryptedItem Ride = DecryptedRide
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data BPPRide = BPPRide {} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EstimatedEndTimeRange = EstimatedEndTimeRange {end :: Kernel.Prelude.UTCTime, start :: Kernel.Prelude.UTCTime} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PaymentStatus = Completed | NotInitiated | Initiated | Cancelled | Failed deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data RideStatus = UPCOMING | NEW | INPROGRESS | COMPLETED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SosJourneyStatus
  = Safe
  | UnexpectedCondition Domain.Types.Ride.UnexpectedConditionStage
  | IVRCallInitiated
  | CSAlerted
  | PoliceMonitoring
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data UnexpectedConditionStage = DriverDeviated | UnusualStop | UnsafeArea deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PaymentStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RideStatus)

$(mkHttpInstancesForEnum ''RideStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SosJourneyStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''UnexpectedConditionStage)
