{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MgEnrollment where

import qualified Data.Time.Calendar
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.MgEnrollment
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data MgEnrollmentT f = MgEnrollmentT
  { beneficiaryId :: B.C f Kernel.Prelude.Text,
    beneficiaryType :: B.C f Domain.Types.MgEnrollment.MgBeneficiaryType,
    campaignId :: B.C f Kernel.Prelude.Text,
    driverId :: B.C f Kernel.Prelude.Text,
    events :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    ledgerEntryId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    metrics :: B.C f Kernel.Prelude.Text,
    optedInAt :: B.C f Kernel.Prelude.UTCTime,
    optedInBy :: B.C f Kernel.Prelude.Text,
    payoutPercent :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    periodDate :: B.C f Data.Time.Calendar.Day,
    settledAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    status :: B.C f Domain.Types.MgEnrollment.MgEnrollmentStatus,
    termsAcceptedAt :: B.C f Kernel.Prelude.UTCTime,
    topUpAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MgEnrollmentT where
  data PrimaryKey MgEnrollmentT f = MgEnrollmentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MgEnrollmentId . id

type MgEnrollment = MgEnrollmentT Identity

$(enableKVPG ''MgEnrollmentT ['id] [['campaignId], ['driverId]])

$(mkTableInstances ''MgEnrollmentT "mg_enrollment")
