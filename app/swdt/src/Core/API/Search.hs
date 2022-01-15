module Core.API.Search where

-- import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Core.ReqTypes
import Core.Search (SearchMessage)
import Servant 

type SearchAPI =
  "seach"
    :> ReqBody '[JSON] (BecknReq SearchMessage)
    :> Post '[JSON] AckResponse

confirmAPI :: Proxy SearchAPI
confirmAPI = Proxy