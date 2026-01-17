{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSTicketBookingPaymentCategory where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSQuoteCategoryType
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSTicketBookingPaymentCategoryT f = FRFSTicketBookingPaymentCategoryT
  { bppItemId :: B.C f Kernel.Prelude.Text,
    category :: B.C f (Kernel.Prelude.Maybe Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType),
    code :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    title :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    tnc :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    finalPrice :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    frfsTicketBookingPaymentId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    offeredPrice :: B.C f Kernel.Types.Common.HighPrecMoney,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    price :: B.C f Kernel.Types.Common.HighPrecMoney,
    quoteId :: B.C f Kernel.Prelude.Text,
    selectedQuantity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSTicketBookingPaymentCategoryT where
  data PrimaryKey FRFSTicketBookingPaymentCategoryT f = FRFSTicketBookingPaymentCategoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSTicketBookingPaymentCategoryId . id

type FRFSTicketBookingPaymentCategory = FRFSTicketBookingPaymentCategoryT Identity

$(enableKVPG ''FRFSTicketBookingPaymentCategoryT ['id] [['frfsTicketBookingPaymentId]])

$(mkTableInstances ''FRFSTicketBookingPaymentCategoryT "frfs_ticket_booking_payment_category")
