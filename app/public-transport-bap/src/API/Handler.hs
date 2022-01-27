module API.Handler where

import API.Beckn.Handler as Beckn
import API.PublicTransport.Handler as PublicTransport
import qualified API.Types as API
import App.Types
import Servant

handler :: FlowServer API.API
handler =
  Beckn.handler
    :<|> PublicTransport.handler