{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Ride where

import Data.Aeson
import qualified Domain.Types.Booking
import qualified Domain.Types.Client
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleServiceTier
import qualified Domain.Types.VehicleVariant
import qualified Kernel.External.Maps
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Confidence
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data Ride = Ride
  { id :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    bppRideId :: Kernel.Types.Id.Id Domain.Types.Ride.BPPRide,
    bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.Ride.Ride,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    clientId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Client.Client),
    fromLocation :: Domain.Types.Location.Location,
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    status :: Domain.Types.Ride.RideStatus,
    driverName :: Kernel.Prelude.Text,
    driverRating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    driverMobileNumber :: Kernel.Prelude.Text,
    driverMobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverRegisteredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Text,
    vehicleColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant,
    vehicleServiceTierType :: Kernel.Prelude.Maybe Domain.Types.VehicleServiceTier.VehicleServiceTierType,
    otp :: Kernel.Prelude.Text,
    endOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    trackingUrl :: Kernel.Prelude.Maybe Kernel.Types.Common.BaseUrl,
    fare :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    totalFare :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    chargeableDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    traveledDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideRating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    allowedEditLocationAttempts :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    driversPreviousRideDropLoc :: Kernel.Prelude.Maybe Kernel.External.Maps.LatLong,
    startOdometerReading :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    endOdometerReading :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    isFreeRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    safetyCheckStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    showDriversPreviousRideDropLoc :: Kernel.Prelude.Bool,
    paymentDone :: Kernel.Prelude.Bool,
    driverAccountId :: Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.AccountId,
    tollConfidence :: Kernel.Prelude.Maybe Kernel.Types.Confidence.Confidence
  }
  deriving (Generic, Show)

data BPPRide = BPPRide {} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RideAPIEntity = RideAPIEntity
  { bppRideId :: Kernel.Types.Id.Id Domain.Types.Ride.BPPRide,
    chargeableRideDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    chargeableRideDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    computedPrice :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    computedPriceWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    createdAt :: Kernel.Prelude.UTCTime,
    driverArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    driverNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverRatings :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    driverRegisteredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    endOdometerReading :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    endOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    isFreeRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    rideEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideOtp :: Kernel.Prelude.Text,
    rideRating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rideStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    shortRideId :: Kernel.Types.Id.ShortId Domain.Types.Ride.Ride,
    startOdometerReading :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    status :: Domain.Types.Ride.RideStatus,
    tollConfidence :: Kernel.Prelude.Maybe Kernel.Types.Confidence.Confidence,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleColor :: Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text,
    vehicleServiceTierType :: Kernel.Prelude.Maybe Domain.Types.VehicleServiceTier.VehicleServiceTierType,
    vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RideStatus = NEW | INPROGRESS | COMPLETED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RideStatus)

$(mkHttpInstancesForEnum ''RideStatus)
