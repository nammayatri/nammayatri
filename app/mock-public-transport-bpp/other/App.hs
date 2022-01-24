module App where

-- import Beckn.Types.Core.Ack (AckResponse)
-- import Beckn.Types.Core (BecknReq (..))

-- import Core.Domain

-- import Data.Aeson.Types
-- import qualified Data.Text as T
-- import Data.Time.Calendar
-- import Data.Time
-- import GHC.Generics

-- import Servant.Client.Core
import Beckn.Prelude
import Core.Confirm
import Core.Context
import Core.OnSearch
import Core.ReqTypes
import Core.Search
import Servant

becknReqC :: (BecknReq ConfirmMessage)
becknReqC = BecknReq example_context (ConfirmMessage example_order)

becknReqS :: (BecknReq SearchMessage)
becknReqS = BecknReq example_context (SearchMessage exampleItent)

becknReqOnS :: (BecknReq OnSearchMessage)
becknReqOnS = BecknReq example_context (OnSearchMessage example_catalog)

type API =
  "becknReqC" :> Get '[JSON] (BecknReq ConfirmMessage)
    :<|> "becknReqS" :> Get '[JSON] (BecknReq SearchMessage)
    :<|> "becknReqOnS" :> Get '[JSON] (BecknReq OnSearchMessage)
    :<|> "confirm" :> ReqBody '[JSON] (BecknReq ConfirmMessage) :> Post '[JSON] (BecknReq ConfirmMessage)
    :<|> "search" :> ReqBody '[JSON] (BecknReq SearchMessage) :> Post '[JSON] (BecknReq SearchMessage)

server :: Server API
server =
  return becknReqC
    :<|> return becknReqS
    :<|> return becknReqOnS
    :<|> echoConfirm
    :<|> echoSearch
  where
    echoConfirm :: (BecknReq ConfirmMessage) -> Handler (BecknReq ConfirmMessage)
    echoConfirm b = return b
    echoSearch :: (BecknReq SearchMessage) -> Handler (BecknReq SearchMessage)
    echoSearch b = return b

api :: Proxy API
api = Proxy

app :: Application
app = serve api server
