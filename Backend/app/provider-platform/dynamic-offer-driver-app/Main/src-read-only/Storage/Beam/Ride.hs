{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Ride where

import qualified Database.Beam as B
import qualified Domain.Types.Ride
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Version
import Tools.Beam.UtilsTH

data RideT f = RideT
  { backendAppVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    bookingId :: B.C f Kernel.Prelude.Text,
    chargeableDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    clientBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    clientOsVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientSdkVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    distanceCalculationFailed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    driverArrivalTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    driverDeviatedFromRoute :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    driverGoHomeRequestId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverId :: B.C f Kernel.Prelude.Text,
    enableFrequentLocationUpdates :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    endOdometerReadingFileId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    endOdometerReadingValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    endOtp :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    fareParametersId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    isFreeRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    numberOfDeviation :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    numberOfOsrmSnapToRoadCalls :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    numberOfSnapToRoadCalls :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    otp :: B.C f Kernel.Prelude.Text,
    pickupDropOutsideOfThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    safetyAlertTriggered :: B.C f Kernel.Prelude.Bool,
    shortId :: B.C f Kernel.Prelude.Text,
    startOdometerReadingFileId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    startOdometerReadingValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    status :: B.C f Domain.Types.Ride.RideStatus,
    tollCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    trackingUrl :: B.C f Kernel.Prelude.Text,
    traveledDistance :: B.C f Kernel.Types.Common.HighPrecMeters,
    tripEndLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    tripEndLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    tripEndTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    tripStartLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    tripStartLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    tripStartTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    uiDistanceCalculationWithAccuracy :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    uiDistanceCalculationWithoutAccuracy :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RideT where
  data PrimaryKey RideT f = RideId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RideId . id

type Ride = RideT Identity

$(enableKVPG ''RideT ['id] [['bookingId], ['driverGoHomeRequestId], ['driverId]])

$(mkTableInstances ''RideT "ride")
