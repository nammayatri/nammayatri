{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FRFSTicketBookingPaymentCategory where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.FRFSQuoteCategoryType
import qualified Kernel.Types.Common
import qualified Database.Beam as B



data FRFSTicketBookingPaymentCategoryT f
    = FRFSTicketBookingPaymentCategoryT {bppItemId :: (B.C f Kernel.Prelude.Text),
                                         category :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType)),
                                         code :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                         description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                         title :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                         tnc :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                         finalPrice :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                                         frfsTicketBookingPaymentId :: (B.C f Kernel.Prelude.Text),
                                         holdId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                         id :: (B.C f Kernel.Prelude.Text),
                                         merchantId :: (B.C f Kernel.Prelude.Text),
                                         merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                                         offeredPrice :: (B.C f Kernel.Types.Common.HighPrecMoney),
                                         currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
                                         price :: (B.C f Kernel.Types.Common.HighPrecMoney),
                                         quoteId :: (B.C f Kernel.Prelude.Text),
                                         seatIds :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
                                         seatLabels :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
                                         selectedQuantity :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                                         createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                         updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FRFSTicketBookingPaymentCategoryT
    where data PrimaryKey FRFSTicketBookingPaymentCategoryT f = FRFSTicketBookingPaymentCategoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = FRFSTicketBookingPaymentCategoryId . id
type FRFSTicketBookingPaymentCategory = FRFSTicketBookingPaymentCategoryT Identity

$(enableKVPG (''FRFSTicketBookingPaymentCategoryT) [('id)] [[('frfsTicketBookingPaymentId)]])

$(mkTableInstances (''FRFSTicketBookingPaymentCategoryT) "frfs_ticket_booking_payment_category")

