{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Invoice where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Common
import Servant
import Servant.Client

data FareBreakup = FareBreakup {price :: Data.Text.Text, title :: Data.Text.Text}
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

type API = ("invoice" :> GetInvoiceInvoice)

type GetInvoiceInvoice =
  ( "invoice" :> MandatoryQueryParam "from" Kernel.Prelude.UTCTime :> MandatoryQueryParam "phoneNumber" Data.Text.Text
      :> MandatoryQueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get '[JSON] [InvoiceRes]
  )

newtype InvoiceAPIs = InvoiceAPIs {getInvoiceInvoice :: Kernel.Prelude.UTCTime -> Data.Text.Text -> Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient [InvoiceRes]}

mkInvoiceAPIs :: (Client EulerHS.Types.EulerClient API -> InvoiceAPIs)
mkInvoiceAPIs invoiceClient = (InvoiceAPIs {..})
  where
    getInvoiceInvoice = invoiceClient

data InvoiceUserActionType
  = GET_INVOICE_INVOICE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON InvoiceUserActionType where
  toJSON GET_INVOICE_INVOICE = Data.Aeson.String "GET_INVOICE_INVOICE"

instance FromJSON InvoiceUserActionType where
  parseJSON (Data.Aeson.String "GET_INVOICE_INVOICE") = pure GET_INVOICE_INVOICE
  parseJSON _ = fail "GET_INVOICE_INVOICE expected"

$(Data.Singletons.TH.genSingletons [''InvoiceUserActionType])
