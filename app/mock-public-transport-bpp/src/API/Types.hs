module API.Types where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import Core.Confirm
import Core.Search
import Core.Status
import Relude
import Servant

type HealthCheckAPI = "health" :> Get '[JSON] Text

type SearchAPI =
  "search" :> ReqBody '[JSON] (BecknReq SearchMessage) :> Post '[JSON] AckResponse

type ConfirmAPI =
  "confirm" :> ReqBody '[JSON] (BecknReq ConfirmMessage) :> Post '[JSON] AckResponse

type StatusAPI =
  "status" :> ReqBody '[JSON] (BecknReq StatusMessage) :> Post '[JSON] AckResponse

type TotalAPI = HealthCheckAPI :<|> SearchAPI :<|> ConfirmAPI :<|> StatusAPI

totalAPI :: Proxy TotalAPI
totalAPI = Proxy
