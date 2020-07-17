module Beckn.External.MyValueFirst.API where

import Beckn.External.MyValueFirst.Types
import Beckn.Types.App
  ( MandatoryQueryParam,
  )
import Data.Text (toLower)
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant

type ServiceAPI =
  "smpp"
    :> "sendsms"
    :> MandatoryQueryParam "username" Text
    :> MandatoryQueryParam "password" Text
    :> MandatoryQueryParam "from" Text
    :> MandatoryQueryParam "to" Text
    :> MandatoryQueryParam "text" Text
    :> MandatoryQueryParam "category" Text
    :> Post '[PlainText] NoContent

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

submitSms SubmitSms {..} =
  void $ ET.client serviceAPI _username _password (show _from) _to _text (toLower $ show _category)
