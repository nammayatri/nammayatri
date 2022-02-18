module API.UI.QuoteConfirm.Types where

import Beckn.Types.Id
import qualified Domain.Endpoints.UI.QuoteConfirm as DConfirm
import qualified Domain.Types.Quote as DQuote
import Servant
import Tools.Auth

type API =
  "quotes"
    :> TokenAuth
    :> Capture "quoteId" (Id DQuote.Quote)
    :> "confirm"
    :> ReqBody '[JSON] DConfirm.QConfirmReq
    :> Post '[JSON] DConfirm.QConfirmRes
