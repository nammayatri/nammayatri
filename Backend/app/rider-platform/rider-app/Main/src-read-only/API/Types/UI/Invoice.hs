{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Invoice where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.Invoice
import Servant
import Tools.Auth

data FareBreakup = FareBreakup {price :: Data.Text.Text, title :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FinanceInvoiceItem = FinanceInvoiceItem
  { invoiceDate :: Kernel.Prelude.UTCTime,
    invoiceNumber :: Data.Text.Text,
    invoiceStatus :: Lib.Finance.Domain.Types.Invoice.InvoiceStatus,
    invoiceType :: Lib.Finance.Domain.Types.Invoice.InvoiceType,
    issuedByAddress :: Kernel.Prelude.Maybe Data.Text.Text,
    issuedByName :: Kernel.Prelude.Maybe Data.Text.Text,
    issuedByTaxNo :: Kernel.Prelude.Maybe Data.Text.Text,
    issuedToAddress :: Kernel.Prelude.Maybe Data.Text.Text,
    issuedToName :: Kernel.Prelude.Maybe Data.Text.Text,
    issuedToTaxNo :: Kernel.Prelude.Maybe Data.Text.Text,
    lineItems :: Kernel.Prelude.Maybe Data.Aeson.Value,
    subtotal :: Kernel.Types.Common.HighPrecMoney,
    taxAmount :: Kernel.Types.Common.HighPrecMoney,
    taxRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    totalAmount :: Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FinanceInvoiceListRes = FinanceInvoiceListRes {invoices :: [FinanceInvoiceItem], totalItems :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data InvoiceRes = InvoiceRes
  { chargeableDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    chargeableDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    date :: Kernel.Prelude.UTCTime,
    destination :: Data.Text.Text,
    driverName :: Data.Text.Text,
    faresList :: [FareBreakup],
    rideEndTime :: Kernel.Prelude.UTCTime,
    rideStartTime :: Kernel.Prelude.UTCTime,
    shortRideId :: Data.Text.Text,
    source :: Data.Text.Text,
    totalAmount :: Data.Text.Text,
    vehicleNumber :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
