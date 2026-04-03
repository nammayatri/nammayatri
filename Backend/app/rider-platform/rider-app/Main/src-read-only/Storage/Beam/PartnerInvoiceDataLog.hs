{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.PartnerInvoiceDataLog where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data PartnerInvoiceDataLogT f
    = PartnerInvoiceDataLogT {bookingId :: (B.C f Kernel.Prelude.Text),
                              createdAt :: (B.C f Kernel.Prelude.UTCTime),
                              exportedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                              id :: (B.C f Kernel.Prelude.Text),
                              merchantId :: (B.C f Kernel.Prelude.Text),
                              merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                              personId :: (B.C f Kernel.Prelude.Text),
                              requestedAt :: (B.C f Kernel.Prelude.UTCTime),
                              updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table PartnerInvoiceDataLogT
    where data PrimaryKey PartnerInvoiceDataLogT f = PartnerInvoiceDataLogId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PartnerInvoiceDataLogId . id
type PartnerInvoiceDataLog = PartnerInvoiceDataLogT Identity

$(enableKVPG (''PartnerInvoiceDataLogT) [('id)] [[('bookingId)]])

$(mkTableInstances (''PartnerInvoiceDataLogT) "partner_invoice_data_log")

