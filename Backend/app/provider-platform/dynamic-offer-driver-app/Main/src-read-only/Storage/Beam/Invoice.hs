{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Invoice where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.Invoice
import qualified Kernel.Types.Common
import qualified Domain.Types.Plan
import qualified Database.Beam as B



data InvoiceT f
    = InvoiceT {bankErrorCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                bankErrorMessage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                bankErrorUpdatedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                createdAt :: (B.C f Kernel.Prelude.UTCTime),
                driverFeeId :: (B.C f Kernel.Prelude.Text),
                driverId :: (B.C f Kernel.Prelude.Text),
                id :: (B.C f Kernel.Prelude.Text),
                invoiceShortId :: (B.C f Kernel.Prelude.Text),
                invoiceStatus :: (B.C f Domain.Types.Invoice.InvoiceStatus),
                lastStatusCheckedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                maxMandateAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                paymentMode :: (B.C f Domain.Types.Invoice.InvoicePaymentMode),
                serviceName :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Plan.ServiceNames)),
                updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table InvoiceT
    where data PrimaryKey InvoiceT f = InvoiceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = InvoiceId . id
type Invoice = InvoiceT Identity

$(enableKVPG (''InvoiceT) [('id)] [[('driverFeeId)], [('driverId)], [('invoiceShortId)]])

$(mkTableInstances (''InvoiceT) "invoice")

