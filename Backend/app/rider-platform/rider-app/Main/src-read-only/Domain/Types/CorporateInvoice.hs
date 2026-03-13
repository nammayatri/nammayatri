{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CorporateInvoice (module Domain.Types.CorporateInvoice, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.CorporateInvoice as ReExport
import qualified Domain.Types.CorporateEntity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CorporateInvoice = CorporateInvoice
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateInvoice.CorporateInvoice,
    corporateEntityId :: Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity,
    invoiceNumber :: Kernel.Prelude.Text,
    periodStart :: Kernel.Prelude.UTCTime,
    periodEnd :: Kernel.Prelude.UTCTime,
    totalTrips :: Kernel.Prelude.Int,
    baseAmount :: Kernel.Types.Common.HighPrecMoney,
    cgstRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    cgstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    sgstRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    sgstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    igstRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    igstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    totalTaxAmount :: Kernel.Types.Common.HighPrecMoney,
    netAmount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    sacCode :: Kernel.Prelude.Text,
    placeOfSupply :: Kernel.Prelude.Text,
    supplierGstin :: Kernel.Prelude.Text,
    recipientGstin :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    eInvoiceIrn :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Domain.Types.CorporateInvoice.CorporateInvoiceStatus,
    pdfUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    generatedAt :: Kernel.Prelude.UTCTime,
    paidAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data CorporateInvoiceStatus = DRAFT | PENDING_APPROVAL | APPROVED | SENT | PAID | DISPUTED | CREDIT_NOTE_ISSUED | OVERDUE
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CorporateInvoiceStatus)
