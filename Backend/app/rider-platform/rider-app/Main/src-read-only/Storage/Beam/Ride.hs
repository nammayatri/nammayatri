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
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Confidence
import qualified Kernel.Types.Version
import Tools.Beam.UtilsTH

data RideT f = RideT
  { id :: B.C f Kernel.Prelude.Text,
    bppRideId :: B.C f Kernel.Prelude.Text,
    bookingId :: B.C f Kernel.Prelude.Text,
    shortId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    status :: B.C f Domain.Types.Ride.RideStatus,
    driverName :: B.C f Kernel.Prelude.Text,
    driverRating :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    driverMobileNumber :: B.C f Kernel.Prelude.Text,
    driverMobileCountryCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverRegisteredAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    driverImage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleNumber :: B.C f Kernel.Prelude.Text,
    vehicleModel :: B.C f Kernel.Prelude.Text,
    vehicleColor :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleVariant :: B.C f Domain.Types.VehicleVariant.VehicleVariant,
    vehicleServiceTierType :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleServiceTier.VehicleServiceTierType),
    otp :: B.C f Kernel.Prelude.Text,
    endOtp :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    trackingUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    fare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    totalFare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    chargeableDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    chargeableDistanceValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    traveledDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    traveledDistanceValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    driverArrivalTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    rideStartTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    rideEndTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    rideRating :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    allowedEditLocationAttempts :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    driversPreviousRideDropLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    driversPreviousRideDropLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    startOdometerReading :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    endOdometerReading :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    isFreeRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    clientOsVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientSdkVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendAppVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    safetyCheckStatus :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    showDriversPreviousRideDropLoc :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    paymentDone :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    driverAccountId :: B.C f (Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.AccountId),
    tollConfidence :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Confidence.Confidence)
  }
  deriving (Generic, B.Beamable)

instance B.Table RideT where
  data PrimaryKey RideT f = RideId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RideId . id

type Ride = RideT Identity

$(enableKVPG ''RideT ['id] [['bppRideId], ['bookingId], ['shortId]])

$(mkTableInstances ''RideT "ride")
