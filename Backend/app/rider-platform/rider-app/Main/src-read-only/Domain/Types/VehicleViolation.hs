{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleViolation where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.TicketService
import qualified IssueManagement.Domain.Types.MediaFile
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehicleViolation = VehicleViolation
  { approvedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    fineAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleViolation.VehicleViolation,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    mediaFileIds :: [Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile],
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    reportedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reporterPersonId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    status :: Domain.Types.VehicleViolation.ViolationStatus,
    ticketServiceId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService),
    vehicleNumber :: Kernel.Prelude.Text,
    violationType :: Domain.Types.VehicleViolation.ViolationType,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ViolationStatus
  = Pending
  | Approved
  | Rejected
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data ViolationType
  = Warning
  | Clamping
  | Towing
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ViolationStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ViolationType)
