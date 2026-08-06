{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.ReconUtrSettlement where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement
import Tools.Beam.UtilsTH

data ReconUtrSettlementT f = ReconUtrSettlementT
  { bankVerifiedAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    bapId :: (B.C f Kernel.Prelude.Text),
    bapUri :: (B.C f Kernel.Prelude.Text),
    claimedTotalAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    deadline :: (B.C f Kernel.Prelude.UTCTime),
    deadlineBreachedNotifiedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    resolutionStatus :: (B.C f Lib.Finance.Domain.Types.ReconUtrSettlement.UtrResolutionStatus),
    resolvedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    resolvedBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    sendAttempts :: (B.C f Kernel.Prelude.Int),
    sendStatus :: (B.C f Lib.Finance.Domain.Types.ReconUtrSettlement.UtrSendStatus),
    sentAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    totalOrders :: (B.C f Kernel.Prelude.Int),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    utr :: (B.C f Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table ReconUtrSettlementT where
  data PrimaryKey ReconUtrSettlementT f = ReconUtrSettlementId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ReconUtrSettlementId . id

type ReconUtrSettlement = ReconUtrSettlementT Identity

$(enableKVPG (''ReconUtrSettlementT) [('id)] [[('utr)]])

$(mkTableInstancesGenericSchema (''ReconUtrSettlementT) "recon_utr_settlement")
