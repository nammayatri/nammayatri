{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Safety.Domain.Types.Sos where

import Data.Aeson
import qualified IssueManagement.Domain.Types.MediaFile
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Safety.Domain.Types.Common
import qualified Tools.Beam.UtilsTH

data Sos = Sos
  { createdAt :: Kernel.Prelude.UTCTime,
    entityType :: Kernel.Prelude.Maybe Safety.Domain.Types.Sos.SosEntityType,
    externalReferenceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    externalReferenceStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    externalStatusHistory :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    flow :: Safety.Domain.Types.Sos.SosType,
    id :: Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos,
    mediaFiles :: [Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile],
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Safety.Domain.Types.Common.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Safety.Domain.Types.Common.MerchantOperatingCity),
    personId :: Kernel.Types.Id.Id Safety.Domain.Types.Common.Person,
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Safety.Domain.Types.Common.Ride),
    sosState :: Kernel.Prelude.Maybe Safety.Domain.Types.Sos.SosState,
    status :: Safety.Domain.Types.Sos.SosStatus,
    ticketId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    trackingExpiresAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EmergencyContactId = EmergencyContactId Kernel.Prelude.Text deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SosEntityType = Ride | NonRide deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SosMockDrill = SosMockDrill {personId :: Kernel.Types.Id.Id Safety.Domain.Types.Common.Person, status :: Safety.Domain.Types.Sos.SosStatus} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SosState = LiveTracking | SosActive deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SosStatus = Resolved | NotResolved | Pending | MockPending | MockResolved deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SosType
  = Police
  | CustomerCare
  | EmergencyContact Safety.Domain.Types.Sos.EmergencyContactId
  | SafetyFlow
  | CSAlertSosTicket
  | AudioRecording
  | KaptureDashboard
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''EmergencyContactId)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SosEntityType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SosState)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SosStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SosType)
