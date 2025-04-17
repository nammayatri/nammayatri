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
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data TripTransaction = TripTransaction
  { allowEndingMidRoute :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    deviationCount :: Kernel.Prelude.Int,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    driverName :: Kernel.Prelude.Maybe Data.Text.Text,
    endLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    endRideApprovalRequestId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.AlertRequest.AlertRequest),
    endStopCode :: Data.Text.Text,
    fleetBadgeId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.FleetBadge.FleetBadge),
    fleetOwnerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction,
    isCurrentlyDeviated :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    roundRouteCode :: Kernel.Prelude.Maybe Data.Text.Text,
    routeCode :: Data.Text.Text,
    startLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    startedNearStopCode :: Kernel.Prelude.Maybe Data.Text.Text,
    status :: Domain.Types.TripTransaction.TripStatus,
    tripCode :: Kernel.Prelude.Maybe Data.Text.Text,
    tripEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tripStartSource :: Kernel.Prelude.Maybe Domain.Types.TripTransaction.ActionSource,
    tripStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tripTerminationSource :: Kernel.Prelude.Maybe Domain.Types.TripTransaction.ActionSource,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleNumber :: Data.Text.Text,
    vehicleServiceTierType :: Domain.Types.Common.ServiceTierType
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ActionSource
  = DriverDirect
  | DriverOnApproval
  | AutoDetect
  | Dashboard
  | ForceDashboard
  | CronJob
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data TripStatus = TRIP_ASSIGNED | CANCELLED | IN_PROGRESS | PAUSED | COMPLETED | UPCOMING deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''TripStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''TripStatus)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''ActionSource)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''ActionSource)
