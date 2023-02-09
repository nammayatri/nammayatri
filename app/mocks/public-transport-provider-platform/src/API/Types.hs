module API.Types where

import "public-transport-rider-platform" Beckn.Spec.Confirm
import "public-transport-rider-platform" Beckn.Spec.Search
import "public-transport-rider-platform" Beckn.Spec.Status
import Kernel.Types.Beckn.Ack
import Kernel.Types.Beckn.ReqTypes
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
