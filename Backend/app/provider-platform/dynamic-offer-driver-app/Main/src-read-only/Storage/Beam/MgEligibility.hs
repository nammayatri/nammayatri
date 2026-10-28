{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MgEligibility where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.MgEligibility
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MgEligibilityT f = MgEligibilityT
  { campaignId :: B.C f Kernel.Prelude.Text,
    driverId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    invitedAt :: B.C f Kernel.Prelude.UTCTime,
    notifiedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    reason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    source :: B.C f Domain.Types.MgEligibility.MgEligibilitySource,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MgEligibilityT where
  data PrimaryKey MgEligibilityT f = MgEligibilityId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MgEligibilityId . id

type MgEligibility = MgEligibilityT Identity

$(enableKVPG ''MgEligibilityT ['id] [['campaignId], ['driverId]])

$(mkTableInstances ''MgEligibilityT "mg_eligibility")
