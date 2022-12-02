module API.Types where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import "public-transport-bap" Core.Spec.Confirm
import "public-transport-bap" Core.Spec.Search
import "public-transport-bap" Core.Spec.Status
import Relude
import Servant

type SearchAPI =
  "search" :> ReqBody '[JSON] (BecknReq SearchMessage) :> Post '[JSON] AckResponse

type ConfirmAPI =
  "confirm" :> ReqBody '[JSON] (BecknReq ConfirmMessage) :> Post '[JSON] AckResponse

type StatusAPI =
  "status" :> ReqBody '[JSON] (BecknReq StatusMessage) :> Post '[JSON] AckResponse

type TotalAPI = SearchAPI :<|> ConfirmAPI :<|> StatusAPI

totalAPI :: Proxy TotalAPI
totalAPI = Proxy
