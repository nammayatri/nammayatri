{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSRecon where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSRecon
import qualified Domain.Types.FRFSTicketStatus
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSReconT f = FRFSReconT
  { beneficiaryBankAccount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    beneficiaryIFSC :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    buyerFinderFee :: B.C f Kernel.Types.Common.HighPrecMoney,
    collectorIFSC :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    collectorSubscriberId :: B.C f Kernel.Prelude.Text,
    date :: B.C f Kernel.Prelude.Text,
    destinationStationCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    differenceAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    entityType :: B.C f (Kernel.Prelude.Maybe Domain.Types.FRFSRecon.EntityType),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    fare :: B.C f Kernel.Types.Common.HighPrecMoney,
    frfsTicketBookingId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    message :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    mobileNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    networkOrderId :: B.C f Kernel.Prelude.Text,
    paymentGateway :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    providerId :: B.C f Kernel.Prelude.Text,
    providerName :: B.C f Kernel.Prelude.Text,
    receiverSubscriberId :: B.C f Kernel.Prelude.Text,
    reconStatus :: B.C f (Kernel.Prelude.Maybe Domain.Types.FRFSRecon.ReconStatus),
    settlementAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    settlementDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    settlementReferenceNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    sourceStationCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    ticketNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    ticketQty :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    ticketStatus :: B.C f (Kernel.Prelude.Maybe Domain.Types.FRFSTicketStatus.FRFSTicketStatus),
    time :: B.C f Kernel.Prelude.Text,
    totalOrderValue :: B.C f Kernel.Types.Common.HighPrecMoney,
    transactionRefNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    transactionUUID :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    txnId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSReconT where
  data PrimaryKey FRFSReconT f = FRFSReconId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSReconId . id

type FRFSRecon = FRFSReconT Identity

$(enableKVPG ''FRFSReconT ['id] [])

$(mkTableInstances ''FRFSReconT "frfs_recon")
