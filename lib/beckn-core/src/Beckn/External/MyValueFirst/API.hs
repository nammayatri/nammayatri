module Beckn.External.MyValueFirst.API where

import Beckn.External.MyValueFirst.Types
import Beckn.Types.App
  ( MandatoryQueryParam,
  )
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
    :> Post '[PlainText] Text

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

submitSms :: SubmitSms -> ET.EulerClient Text
submitSms SubmitSms {..} = ET.client serviceAPI username password from to text
