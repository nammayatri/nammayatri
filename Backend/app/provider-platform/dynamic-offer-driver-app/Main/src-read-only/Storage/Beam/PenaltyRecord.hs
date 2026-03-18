{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PenaltyRecord where

import qualified Database.Beam as B
import qualified Domain.Types.PenaltyRecord
import qualified Domain.Types.PenaltyRule
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data PenaltyRecordT f = PenaltyRecordT
  { id :: B.C f Kernel.Prelude.Text,
    driverId :: B.C f Kernel.Prelude.Text,
    ruleId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    triggerEvent :: B.C f Domain.Types.PenaltyRule.PenaltyTriggerEvent,
    triggerEntityId :: B.C f Kernel.Prelude.Text,
    amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    reason :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.PenaltyRecord.PenaltyStatus,
    disputeReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    disputeEvidence :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    disputeResolvedBy :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    disputeResolvedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    ledgerEntryId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    invoiceId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PenaltyRecordT where
  data PrimaryKey PenaltyRecordT f = PenaltyRecordId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PenaltyRecordId . id

type PenaltyRecord = PenaltyRecordT Identity

$(enableKVPG ''PenaltyRecordT ['id] [['driverId], ['ruleId]])

$(mkTableInstances ''PenaltyRecordT "penalty_record")
