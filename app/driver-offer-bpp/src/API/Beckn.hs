module API.Beckn (API, handler) where

import qualified API.Beckn.Cancel as Cancel
import qualified API.Beckn.Confirm as Confirm
import qualified API.Beckn.Init as Init
import qualified API.Beckn.Rating as Rating
import qualified API.Beckn.Search as Search
import qualified API.Beckn.Select as Select
import qualified API.Beckn.Track as Track
import Environment
import Servant

type API =
  "beckn"
    :> ( Search.API
           :<|> Select.API
           :<|> Init.API
           :<|> Confirm.API
           :<|> Track.API
           :<|> Cancel.API
           :<|> Rating.API
       )

handler :: FlowServer API
handler =
  Search.handler
    :<|> Select.handler
    :<|> Init.handler
    :<|> Confirm.handler
    :<|> Track.handler
    :<|> Cancel.handler
    :<|> Rating.handler
