module Beckn.Types.Core.Taxi.API.Select where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes (BecknReq)
import Beckn.Types.Core.Taxi.Select (SelectMessage)
import Servant (JSON, Post, ReqBody, (:>))

type SelectReq = BecknReq SelectMessage

type SelectRes = AckResponse

type SelectAPI =
  "select"
    :> ReqBody '[JSON] SelectReq
    :> Post '[JSON] SelectRes

selectAPI :: Proxy SelectAPI
selectAPI = Proxy
