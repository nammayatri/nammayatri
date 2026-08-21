{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.LedgerAdjustmentRequest where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.LedgerAdjustmentRequest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data LedgerAdjustmentRequestT f = LedgerAdjustmentRequestT
  { adminCheckerId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    adminCheckerName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    adminMakerId :: B.C f Kernel.Prelude.Text,
    adminMakerName :: B.C f Kernel.Prelude.Text,
    amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    approvedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    category :: B.C f Domain.Types.LedgerAdjustmentRequest.AdjustmentCategory,
    currency :: B.C f Kernel.Types.Common.Currency,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    direction :: B.C f Domain.Types.LedgerAdjustmentRequest.AdjustmentDirection,
    documentId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    errorMessage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    ledgerEntryId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    postedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    referenceId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    referenceType :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.LedgerAdjustmentRequest.AdjustmentRequestStatus,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table LedgerAdjustmentRequestT where
  data PrimaryKey LedgerAdjustmentRequestT f = LedgerAdjustmentRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = LedgerAdjustmentRequestId . id

type LedgerAdjustmentRequest = LedgerAdjustmentRequestT Identity

$(enableKVPG ''LedgerAdjustmentRequestT ['id] [['referenceId]])

$(mkTableInstances ''LedgerAdjustmentRequestT "ledger_adjustment_request")
