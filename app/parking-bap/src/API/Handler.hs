module API.Handler where

import qualified API.Beckn.Handler as Beckn
import qualified API.Parking.Handler as Parking
import qualified API.Types as API
import App.Types
import Servant

handler :: FlowServer API.API
handler =
  Beckn.handler
    :<|> Parking.handler
