{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSTicket where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSTicketStatus
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSTicketT f = FRFSTicketT
  { commencingHours :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    frfsTicketBookingId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    isTicketFree :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    partnerOrgId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    partnerOrgTransactionId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    qrData :: B.C f Kernel.Prelude.Text,
    qrRefreshAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    riderId :: B.C f Kernel.Prelude.Text,
    scannedByVehicleNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    status :: B.C f Domain.Types.FRFSTicketStatus.FRFSTicketStatus,
    ticketNumber :: B.C f Kernel.Prelude.Text,
    validTill :: B.C f Kernel.Prelude.UTCTime,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSTicketT where
  data PrimaryKey FRFSTicketT f = FRFSTicketId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSTicketId . id

type FRFSTicket = FRFSTicketT Identity

$(enableKVPG ''FRFSTicketT ['id] [['frfsTicketBookingId]])

$(mkTableInstances ''FRFSTicketT "frfs_ticket")
