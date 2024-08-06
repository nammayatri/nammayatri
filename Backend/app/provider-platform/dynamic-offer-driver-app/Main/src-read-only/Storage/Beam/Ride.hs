{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Ride where

import qualified Database.Beam as B
import qualified Domain.Types.Common
import qualified Domain.Types.Ride
import qualified Domain.Types.Vehicle
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Confidence
import qualified Kernel.Types.Version
import Tools.Beam.UtilsTH

data RideT f = RideT
  { backendAppVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    bookingId :: B.C f Kernel.Prelude.Text,
    cancellationFeeIfCancelled :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    chargeableDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    chargeableRideDurationInMinutes :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    clientBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientManufacturer :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientModelName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    clientOsVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientSdkVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    distanceCalculationFailed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    driverArrivalTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    driverDeviatedFromRoute :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    driverDeviatedToTollRoute :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    driverGoHomeRequestId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverId :: B.C f Kernel.Prelude.Text,
    enableFrequentLocationUpdates :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    enableOtpLessRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    endOdometerReadingFileId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    endOdometerReadingValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    endOtp :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    estimatedTollCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    estimatedTollNames :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    fare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    fareAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    fareParametersId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    isAdvanceBooking :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isAirConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isFreeRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    numberOfDeviation :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    numberOfOsrmSnapToRoadCalls :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    numberOfSelfTuned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    numberOfSnapToRoadCalls :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    onlinePayment :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    otp :: B.C f Kernel.Prelude.Text,
    pickupDropOutsideOfThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    previousRideTripEndLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    previousRideTripEndLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    previousRideTripEndTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    rideEndedBy :: B.C f (Kernel.Prelude.Maybe Domain.Types.Ride.RideEndedBy),
    safetyAlertTriggered :: B.C f Kernel.Prelude.Bool,
    shortId :: B.C f Kernel.Prelude.Text,
    startOdometerReadingFileId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    startOdometerReadingValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    status :: B.C f Domain.Types.Ride.RideStatus,
    tollCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    tollConfidence :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Confidence.Confidence),
    tollNames :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    trackingUrl :: B.C f Kernel.Prelude.Text,
    traveledDistance :: B.C f Kernel.Types.Common.HighPrecMeters,
    tripCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.Common.TripCategory),
    tripEndLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    tripEndLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    tripEndTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    tripStartLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    tripStartLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    tripStartTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    uiDistanceCalculationWithAccuracy :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    uiDistanceCalculationWithoutAccuracy :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleServiceTierAirConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    vehicleServiceTierName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleServiceTierSeatingCapacity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    vehicleVariant :: B.C f (Kernel.Prelude.Maybe Domain.Types.Vehicle.Variant)
  }
  deriving (Generic, B.Beamable)

instance B.Table RideT where
  data PrimaryKey RideT f = RideId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RideId . id

type Ride = RideT Identity

$(enableKVPG ''RideT ['id] [['bookingId], ['driverGoHomeRequestId], ['driverId]])

$(mkTableInstances ''RideT "ride")
