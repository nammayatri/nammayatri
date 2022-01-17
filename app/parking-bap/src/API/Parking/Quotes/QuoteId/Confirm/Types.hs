module API.Parking.Quotes.QuoteId.Confirm.Types where

import Beckn.Prelude
import Beckn.Types.Id
import Domain.Booking (Booking)
import Servant
import Tools.Auth

type API =
  "confirm"
    :> PostQuoteConfirmAPI

type PostQuoteConfirmAPI =
  TokenAuth
    :> ReqBody '[JSON] PostQuoteConfirmReq
    :> Post '[JSON] PostQuoteConfirmRes

data PostQuoteConfirmReq = PostQuoteConfirmReq
  { requestorNumber :: Text,
    vehicleNumber :: Text,
    requestorName :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype PostQuoteConfirmRes = PostQuoteConfirmRes
  { bookingId :: Id Booking
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, ToSchema)
