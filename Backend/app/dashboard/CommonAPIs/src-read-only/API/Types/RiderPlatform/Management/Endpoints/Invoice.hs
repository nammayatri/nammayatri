{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Invoice where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
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

data InvoiceEndpointDSL
  = GetInvoiceInvoiceEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON InvoiceEndpointDSL where
  toJSON GetInvoiceInvoiceEndpoint = Data.Aeson.String "GetInvoiceInvoiceEndpoint"

instance FromJSON InvoiceEndpointDSL where
  parseJSON (Data.Aeson.String "GetInvoiceInvoiceEndpoint") = pure GetInvoiceInvoiceEndpoint
  parseJSON _ = fail "GetInvoiceInvoiceEndpoint expected"
