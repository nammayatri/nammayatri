{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Sos where

import qualified IssueManagement.Common
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id

data Sos = Sos
  { flow :: Domain.Types.Sos.SosType,
    id :: Kernel.Types.Id.Id Domain.Types.Sos.Sos,
    personId :: Kernel.Types.Id.Id IssueManagement.Common.Person,
    rideId :: Kernel.Types.Id.Id IssueManagement.Common.Ride,
    status :: Domain.Types.Sos.SosStatus,
    ticketId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Common.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Common.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EmergencyContactId = EmergencyContactId Kernel.Prelude.Text deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SosMockDrill = SosMockDrill {personId :: Kernel.Types.Id.Id IssueManagement.Common.Person, status :: Domain.Types.Sos.SosStatus} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SosStatus = Resolved | NotResolved | Pending | MockPending | MockResolved deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SosType = Police | CustomerCare | EmergencyContact Domain.Types.Sos.EmergencyContactId | SafetyFlow deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''SosType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''SosStatus))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''EmergencyContactId))
