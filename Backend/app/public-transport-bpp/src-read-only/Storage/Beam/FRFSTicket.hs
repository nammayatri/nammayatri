{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSTicket where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSTicket
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSTicketT f = FRFSTicketT
  { bapId :: (B.C f Kernel.Prelude.Text),
    bookingId :: (B.C f Kernel.Prelude.Text),
    bppId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    ticketQr :: (B.C f Kernel.Prelude.Text),
    ticketStatus :: (B.C f Domain.Types.FRFSTicket.TicketStatusEnum),
    transactionId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    validTill :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSTicketT where
  data PrimaryKey FRFSTicketT f = FRFSTicketId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSTicketId . id

type FRFSTicket = FRFSTicketT Identity

$(enableKVPG (''FRFSTicketT) [('id)] [])

$(mkTableInstances (''FRFSTicketT) "frfs_ticket")
