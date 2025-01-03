{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSTicketDiscount where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSTicketDiscount
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSTicketDiscountT f = FRFSTicketDiscountT
  { code :: B.C f Kernel.Prelude.Text,
    currency :: B.C f Kernel.Types.Common.Currency,
    description :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    title :: B.C f Kernel.Prelude.Text,
    tnc :: B.C f Kernel.Prelude.Text,
    value :: B.C f Domain.Types.FRFSTicketDiscount.DiscountValue,
    vehicleType :: B.C f BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSTicketDiscountT where
  data PrimaryKey FRFSTicketDiscountT f = FRFSTicketDiscountId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSTicketDiscountId . id

type FRFSTicketDiscount = FRFSTicketDiscountT Identity

$(enableKVPG ''FRFSTicketDiscountT ['id] [])

$(mkTableInstances ''FRFSTicketDiscountT "frfs_ticket_discount")
