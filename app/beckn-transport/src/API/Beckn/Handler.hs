module API.Beckn.Handler where

import qualified API.Beckn.Cancel as Cancel
import qualified API.Beckn.Confirm as Confirm
import qualified API.Beckn.Init as Init
import qualified API.Beckn.Rating as Rating
import qualified API.Beckn.Search as Search
import App.Types
import Servant

type API =
  Search.API
    :<|> Init.API
    :<|> Confirm.API
    :<|> Cancel.API
    :<|> Rating.API

handler :: FlowServer API
handler =
  Search.handler
    :<|> Init.handler
    :<|> Confirm.handler
    :<|> Cancel.handler
    :<|> Rating.handler
