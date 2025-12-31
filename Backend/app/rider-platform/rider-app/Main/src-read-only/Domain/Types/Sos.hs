{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Sos where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified IssueManagement.Domain.Types.MediaFile
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Sos = Sos
  { flow :: Domain.Types.Sos.SosType,
    id :: Kernel.Types.Id.Id Domain.Types.Sos.Sos,
    mediaFiles :: [Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile],
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride),
    status :: Domain.Types.Sos.SosStatus,
    ticketId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    trackingExpiresAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EmergencyContactId = EmergencyContactId Kernel.Prelude.Text deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SosMockDrill = SosMockDrill {personId :: Kernel.Types.Id.Id Domain.Types.Person.Person, status :: Domain.Types.Sos.SosStatus} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SosStatus = Resolved | NotResolved | Pending | MockPending | MockResolved deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SosType
  = Police
  | CustomerCare
  | EmergencyContact Domain.Types.Sos.EmergencyContactId
  | SafetyFlow
  | CSAlertSosTicket
  | AudioRecording
  | KaptureDashboard
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''EmergencyContactId)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SosStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SosType)
