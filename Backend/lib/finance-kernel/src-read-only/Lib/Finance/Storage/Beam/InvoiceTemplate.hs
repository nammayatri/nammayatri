{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.InvoiceTemplate where

import qualified Database.Beam as B
import qualified Domain.Types.Invoice
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data InvoiceTemplateT f = InvoiceTemplateT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    invoiceType :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Invoice.InvoiceType)),
    language :: (B.C f Kernel.External.Types.Language),
    lineItemRowTemplate :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    template :: (B.C f Kernel.Prelude.Text),
    totalsLineRowTemplate :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table InvoiceTemplateT where
  data PrimaryKey InvoiceTemplateT f = InvoiceTemplateId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = InvoiceTemplateId . id

type InvoiceTemplate = InvoiceTemplateT Identity

$(enableKVPG (''InvoiceTemplateT) [('id)] [])

$(mkTableInstancesGenericSchema (''InvoiceTemplateT) "invoice_template")
