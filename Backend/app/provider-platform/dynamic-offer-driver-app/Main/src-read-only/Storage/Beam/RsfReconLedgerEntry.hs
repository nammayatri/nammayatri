{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RsfReconLedgerEntry where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.RsfReconLedgerEntry
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data RsfReconLedgerEntryT f = RsfReconLedgerEntryT
  { actorId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    actorType :: (B.C f Domain.Types.RsfReconLedgerEntry.RsfActorType),
    amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    bapId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    bapUri :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    bffAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    bffType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    claimStatus :: (B.C f (Kernel.Prelude.Maybe Domain.Types.RsfReconLedgerEntry.RsfClaimStatus)),
    collectorAppId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    contextTransactionId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    counterpartyReconStatus :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f Kernel.Prelude.Text),
    deductionByCollector :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    diffMessageCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    diffMessageName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    driverId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    effectiveAt :: (B.C f Kernel.Prelude.UTCTime),
    entryType :: (B.C f Domain.Types.RsfReconLedgerEntry.RsfLedgerEntryType),
    id :: (B.C f Kernel.Prelude.Text),
    invoiceNo :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    messageId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    orderId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    orderPaymentAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    orderState :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    orderTransactionId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    rawJson :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    reason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    rejectionDiff :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    reportedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    reportedInMessageId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    rideId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    settlementId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    settlementReasonCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    source :: (B.C f Domain.Types.RsfReconLedgerEntry.RsfLedgerSource),
    ttl :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    utr :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    withholdingTaxGst :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    withholdingTaxTds :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney))
  }
  deriving (Generic, B.Beamable)

instance B.Table RsfReconLedgerEntryT where
  data PrimaryKey RsfReconLedgerEntryT f = RsfReconLedgerEntryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RsfReconLedgerEntryId . id

type RsfReconLedgerEntry = RsfReconLedgerEntryT Identity

$(enableKVPG (''RsfReconLedgerEntryT) [('id)] [[('messageId)], [('orderId)], [('utr)]])

$(mkTableInstances (''RsfReconLedgerEntryT) "rsf_recon_ledger_entry")
