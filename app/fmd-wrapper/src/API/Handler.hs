module API.Handler where

import qualified API.Cancel.Handler as Cancel
import qualified API.Confirm.Handler as Confirm
import qualified API.Search.Handler as Search
import qualified API.Status.Handler as Status
import qualified API.Track.Handler as Track
import qualified API.Types as API
import App.Types
import EulerHS.Prelude
import Servant

handler :: FlowServer API.API
handler =
  pure "FMD wrapper backend is UP"
    :<|> Search.handler
    :<|> Confirm.handler
    :<|> Status.handler
    :<|> Track.handler
    :<|> Cancel.handler
