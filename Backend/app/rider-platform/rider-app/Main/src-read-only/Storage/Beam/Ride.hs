{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Ride where

import qualified Database.Beam as B
import qualified Domain.Types.Ride
import qualified Domain.Types.VehicleServiceTier
import qualified Domain.Types.VehicleVariant
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Confidence
import qualified Kernel.Types.Time
import qualified Kernel.Types.Version
import Tools.Beam.UtilsTH

data RideT f = RideT
  { allowedEditLocationAttempts :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    allowedEditPickupLocationAttempts :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    backendAppVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    bookingId :: B.C f Kernel.Prelude.Text,
    bppRideId :: B.C f Kernel.Prelude.Text,
    cancellationFeeIfCancelled :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    chargeableDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    chargeableDistanceValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    clientBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientManufacturer :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientModelName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    clientOsVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientSdkVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    driverAccountId :: B.C f (Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.AccountId),
    driverAlternateNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverAlternateNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    driverArrivalTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    driverImage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverMobileCountryCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverName :: B.C f Kernel.Prelude.Text,
    driverNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    driverRating :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    driverRegisteredAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    driversPreviousRideDropLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    driversPreviousRideDropLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    endOdometerReading :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    endOtp :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    fare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    favCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    id :: B.C f Kernel.Prelude.Text,
    isAlreadyFav :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isFreeRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    onlinePayment :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    otp :: B.C f Kernel.Prelude.Text,
    paymentDone :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    rideEndTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    rideRating :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    rideStartTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    safetyCheckStatus :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    safetyJourneyStatus :: B.C f (Kernel.Prelude.Maybe Domain.Types.Ride.SosJourneyStatus),
    shortId :: B.C f Kernel.Prelude.Text,
    showDriversPreviousRideDropLoc :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    startOdometerReading :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    status :: B.C f Domain.Types.Ride.RideStatus,
    tollConfidence :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Confidence.Confidence),
    totalFare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    trackingUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    traveledDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    traveledDistanceValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleAge :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Time.Months),
    vehicleColor :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleModel :: B.C f Kernel.Prelude.Text,
    vehicleNumber :: B.C f Kernel.Prelude.Text,
    vehicleServiceTierType :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleServiceTier.VehicleServiceTierType),
    vehicleVariant :: B.C f Domain.Types.VehicleVariant.VehicleVariant
  }
  deriving (Generic, B.Beamable)

instance B.Table RideT where
  data PrimaryKey RideT f = RideId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RideId . id

type Ride = RideT Identity

$(enableKVPG ''RideT ['id] [['bookingId], ['bppRideId], ['shortId]])

$(mkTableInstances ''RideT "ride")
