{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TripTransaction where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.AlertRequest
import qualified Domain.Types.Common
import qualified Domain.Types.FleetBadge
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data TripTransaction = TripTransaction
  { allowEndingMidRoute :: Kernel.Prelude.Bool,
    conductorFleetBadgeId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.FleetBadge.FleetBadge),
    conductorName :: Kernel.Prelude.Maybe Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    deviationCount :: Kernel.Prelude.Int,
    driverFleetBadgeId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.FleetBadge.FleetBadge),
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    driverName :: Kernel.Prelude.Maybe Data.Text.Text,
    dutyType :: Kernel.Prelude.Maybe Data.Text.Text,
    endAddress :: Kernel.Prelude.Maybe Data.Text.Text,
    endLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    endRideApprovalRequestId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.AlertRequest.AlertRequest),
    endStopCode :: Data.Text.Text,
    fleetOwnerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction,
    isCurrentlyDeviated :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    pilotDestination :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    pilotSource :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    roundRouteCode :: Kernel.Prelude.Maybe Data.Text.Text,
    routeCode :: Data.Text.Text,
    scheduledTripTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    startAddress :: Kernel.Prelude.Maybe Data.Text.Text,
    startLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    startedNearStopCode :: Kernel.Prelude.Maybe Data.Text.Text,
    status :: Domain.Types.TripTransaction.TripStatus,
    tripCode :: Kernel.Prelude.Maybe Data.Text.Text,
    tripEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tripEstimatedRouteDetails :: Kernel.Prelude.Maybe Domain.Types.TripTransaction.EstimatedRouteDetails,
    tripStartSource :: Kernel.Prelude.Maybe Domain.Types.TripTransaction.ActionSource,
    tripStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tripTerminationSource :: Kernel.Prelude.Maybe Domain.Types.TripTransaction.ActionSource,
    tripType :: Kernel.Prelude.Maybe Domain.Types.TripTransaction.TripType,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleNumber :: Data.Text.Text,
    vehicleServiceTierType :: Domain.Types.Common.ServiceTierType,
    vipName :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ActionSource
  = DriverDirect
  | DriverOnApproval
  | AutoDetect
  | Dashboard
  | ForceDashboard
  | CronJob
  | AutoRecovery
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data EstimatedRouteDetails = EstimatedRouteDetails {distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters, duration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds, polyline :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data TripStatus = TRIP_ASSIGNED | CANCELLED | IN_PROGRESS | PAUSED | COMPLETED | UPCOMING deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data TripType = PILOT | WIMB deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''TripStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''TripStatus)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''ActionSource)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''ActionSource)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''TripType)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''TripType)
