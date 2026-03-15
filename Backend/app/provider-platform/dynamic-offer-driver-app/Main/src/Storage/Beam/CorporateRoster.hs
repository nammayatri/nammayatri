{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CorporateRoster where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Data.Time.Calendar
import Tools.Beam.UtilsTH

data CorporateRosterT f = CorporateRosterT
  { id :: B.C f Kernel.Prelude.Text,
    corporateEntityId :: B.C f Kernel.Prelude.Text,
    corporateEmployeeId :: B.C f Kernel.Prelude.Text,
    corporateShiftId :: B.C f Kernel.Prelude.Text,
    corporateRouteId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    rosterDate :: B.C f Data.Time.Calendar.Day,
    attendanceStatus :: B.C f Kernel.Prelude.Text,
    bookingId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    confirmedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CorporateRosterT where
  data PrimaryKey CorporateRosterT f = CorporateRosterId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CorporateRosterId . id

type CorporateRoster = CorporateRosterT Identity

$(enableKVPG ''CorporateRosterT ['id] [['corporateEntityId], ['corporateEmployeeId], ['corporateShiftId]])

$(mkTableInstances ''CorporateRosterT "corporate_roster")
