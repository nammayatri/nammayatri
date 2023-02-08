module API.UI.QuoteConfirm.Types where

import qualified Domain.Action.UI.QuoteConfirm as DConfirm
import qualified Domain.Types.Quote as DQuote
import Kernel.Types.Id
import Servant
import Tools.Auth

type API =
  "quotes"
    :> TokenAuth
    :> Capture "quoteId" (Id DQuote.Quote)
    :> "confirm"
    :> ReqBody '[JSON] DConfirm.QConfirmReq
    :> Post '[JSON] DConfirm.QConfirmRes
