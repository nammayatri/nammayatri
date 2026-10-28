{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MgEnrollment where

import Data.Aeson
import qualified Data.Time.Calendar
import qualified Domain.Types.Extra.MgEnrollment
import qualified Domain.Types.MgCampaign
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MgEnrollment = MgEnrollment
  { beneficiaryId :: Kernel.Prelude.Text,
    beneficiaryType :: Domain.Types.MgEnrollment.MgBeneficiaryType,
    campaignId :: Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    events :: [Domain.Types.MgEnrollment.MgEvent],
    id :: Kernel.Types.Id.Id Domain.Types.MgEnrollment.MgEnrollment,
    ledgerEntryId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    metrics :: Domain.Types.Extra.MgEnrollment.MgMetrics,
    optedInAt :: Kernel.Prelude.UTCTime,
    optedInBy :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    payoutPercent :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    periodDate :: Data.Time.Calendar.Day,
    settledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Domain.Types.MgEnrollment.MgEnrollmentStatus,
    termsAcceptedAt :: Kernel.Prelude.UTCTime,
    topUpAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data MgBeneficiaryType = DRIVER | FLEET_OWNER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data MgEnrollmentStatus = UPCOMING | SETTLED | DISQUALIFIED | OPTED_OUT | CANCELLED_BY_ADMIN deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data MgEvent = MgEvent
  { detail :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    eventType :: Domain.Types.MgEnrollment.MgEventType,
    occurredAt :: Kernel.Prelude.UTCTime,
    payoutPercentAfter :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data MgEventType
  = BREAK_STARTED
  | BREAK_ENDED
  | DRIVER_CANCELLED
  | RIDE_COUNTED
  | FLEET_CHANGED
  | DAY_DISQUALIFIED
  | DAY_CANCELLED_BY_ADMIN
  | DAY_SETTLED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''MgBeneficiaryType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''MgEnrollmentStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''MgEventType)
