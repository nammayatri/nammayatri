module API.Types where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import qualified Core.Init.Order as Init
import qualified Core.Search.Message as Search
import Data.Aeson
import qualified Data.Text as T
import Servant

type HealthCheckAPI = "health" :> Get '[JSON] T.Text

type SearchAPI =
  "search" :> ReqBody '[JSON] (BecknReq Search.Message) :> Post '[JSON] AckResponse

type InitAPI =
  "init" :> ReqBody '[JSON] (BecknReq Init.InitMessage) :> Post '[JSON] AckResponse

type ConfirmAPI =
  "confirm" :> ReqBody '[JSON] (BecknReq Value) :> Post '[JSON] AckResponse

type TotalAPI = HealthCheckAPI :<|> SearchAPI :<|> InitAPI -- :<|> ConfirmAPI

totalAPI :: Proxy TotalAPI
totalAPI = Proxy
