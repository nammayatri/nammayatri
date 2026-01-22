{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Ride where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.Ride
import qualified Domain.Types.VehicleVariant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Confidence
import qualified Kernel.Types.Version
import qualified SharedLogic.Type
import Tools.Beam.UtilsTH

data RideT f = RideT
  { backendAppVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    billingCategory :: B.C f (Kernel.Prelude.Maybe SharedLogic.Type.BillingCategory),
    bookingId :: B.C f Kernel.Prelude.Text,
    cancellationChargesOnCancel :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    cancellationFeeIfCancelled :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    chargeableDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    clientBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientManufacturer :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientModelName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    clientOsVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientSdkVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    cloudType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.CloudType),
    commission :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    deliveryFileIds :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    destinationReachedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    distanceCalculationFailed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    driverArrivalTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    driverCancellationPenaltyAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    driverCancellationPenaltyFeeId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverCancellationPenaltyWaivedReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverDeviatedFromRoute :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    driverDeviatedToTollRoute :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    driverGoHomeRequestId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverGpsTurnedOff :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    driverId :: B.C f Kernel.Prelude.Text,
    enableFrequentLocationUpdates :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    enableOtpLessRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    endOdometerReadingFileId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    endOdometerReadingValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    endOtp :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    estimatedEndTimeRangeEnd :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    estimatedEndTimeRangeStart :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    estimatedTollCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    estimatedTollIds :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    estimatedTollNames :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    fare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    fareAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    fareParametersId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    finalFarePolicyId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fleetOwnerId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    hasStops :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    id :: B.C f Kernel.Prelude.Text,
    insuredAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    isAdvanceBooking :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isAirConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isDriverSpecialLocWarrior :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isFreeRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isInsured :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isPetRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isPickupOrDestinationEdited :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    numberOfDeviation :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    numberOfOsrmSnapToRoadCalls :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    numberOfSelfTuned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    numberOfSnapToRoadCalls :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    onlinePayment :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    otp :: B.C f Kernel.Prelude.Text,
    passedThroughDestination :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    pickupDropOutsideOfThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    previousRideTripEndLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    previousRideTripEndLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    previousRideTripEndTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    reactBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    rideEndedBy :: B.C f (Kernel.Prelude.Maybe Domain.Types.Ride.RideEndedBy),
    rideTags :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    safetyAlertTriggered :: B.C f Kernel.Prelude.Bool,
    shortId :: B.C f Kernel.Prelude.Text,
    startOdometerReadingFileId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    startOdometerReadingValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    status :: B.C f Domain.Types.Ride.RideStatus,
    tipAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    tollCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    tollConfidence :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Confidence.Confidence),
    tollIds :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
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
    vehicleVariant :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleVariant.VehicleVariant)
  }
  deriving (Generic, B.Beamable)

instance B.Table RideT where
  data PrimaryKey RideT f = RideId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RideId . id

type Ride = RideT Identity

$(enableKVPG ''RideT ['id] [['bookingId], ['driverGoHomeRequestId], ['driverId]])

$(mkTableInstances ''RideT "ride")
