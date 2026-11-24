{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.OccupancyAlert where

import Data.Aeson
import qualified Domain.Types.DetectionSource
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketService
import qualified Domain.Types.VehicleViolation
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data OccupancyAlert = OccupancyAlert
  { acknowledgedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    category :: Domain.Types.OccupancyAlert.AlertCategory,
    detectedAt :: Kernel.Prelude.UTCTime,
    detectionSourceId :: Kernel.Types.Id.Id Domain.Types.DetectionSource.DetectionSource,
    externalAlertId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    graceUntil :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.OccupancyAlert.OccupancyAlert,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    resolution :: Kernel.Prelude.Maybe Domain.Types.OccupancyAlert.AlertResolution,
    resolvedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Domain.Types.OccupancyAlert.AlertStatus,
    ticketServiceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    unpaidEstimate :: Kernel.Prelude.Int,
    violationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.VehicleViolation.VehicleViolation),
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data AlertCategory
  = Unpaid
  | WrongVehicleType
  | Orientation
  | VehicleType
  | DoublePark
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data AlertResolution
  = PaidLater
  | VehicleLeft
  | VerifiedViolation
  | FalsePositive
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data AlertStatus
  = Pending
  | Open
  | Resolved
  | Dismissed
  | Escalated
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''AlertCategory)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''AlertResolution)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''AlertStatus)
