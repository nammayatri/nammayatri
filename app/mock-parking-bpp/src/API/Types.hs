module API.Types where

import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Core.ReqTypes
import qualified Core.API.Confirm as BAP
import qualified Core.API.Status as BAP
import qualified Core.Search as BAP
import Relude
import Servant

type SearchAPI =
  "search" :> ReqBody '[JSON] (BecknReq BAP.SearchIntent) :> Post '[JSON] AckResponse

type ConfirmAPI = BAP.ConfirmAPI

type StatusAPI = BAP.StatusAPI

type TotalAPI = SearchAPI :<|> ConfirmAPI :<|> StatusAPI

totalAPI :: Proxy TotalAPI
totalAPI = Proxy
