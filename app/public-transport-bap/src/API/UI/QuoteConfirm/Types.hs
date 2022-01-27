module API.UI.QuoteConfirm.Types where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Quote as DQuote
import Servant
import Tools.Auth

type API =
  "quotes"
    :> TokenAuth
    :> Capture "quoteId" (Id DQuote.Quote)
    :> "confirm"
    :> ReqBody '[JSON] QConfirmReq
    :> Post '[JSON] QConfirmRes

data QConfirmReq = QConfirmReq
  { quantity :: Int,
    requestorName :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype QConfirmRes = QConfirmRes
  { booking_id :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
