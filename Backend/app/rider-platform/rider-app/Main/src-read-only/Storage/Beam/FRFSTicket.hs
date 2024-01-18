{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSTicket where

import qualified Database.Beam as B
import qualified Domain.Types.FRFSTicket
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data FRFSTicketT f = FRFSTicketT
  { frfsTicketBookingId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    qrData :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.FRFSTicket.FRFSTicketStatus,
    ticketNumber :: B.C f Kernel.Prelude.Text,
    validTill :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSTicketT where
  data PrimaryKey FRFSTicketT f = FRFSTicketId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = FRFSTicketId . id

type FRFSTicket = FRFSTicketT Identity

$(enableKVPG ''FRFSTicketT ['id] [])

$(mkTableInstances ''FRFSTicketT "frfs_ticket")
