module Beckn.Types.Core.Taxi.API.Select where

import Beckn.Types.Core.Taxi.Select (SelectMessage)
import Kernel.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Servant (JSON, Post, ReqBody, (:>))

type SelectReq = BecknReq SelectMessage

type SelectRes = AckResponse

type SelectAPI =
  "select"
    :> ReqBody '[JSON] SelectReq
    :> Post '[JSON] SelectRes

selectAPI :: Proxy SelectAPI
selectAPI = Proxy
