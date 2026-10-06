{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module InMemManagement
  ( withInMemManagement,
  )
where

import Environment (HandlerEnv)
import qualified EulerHS.Runtime as R
import Kernel.Prelude
import Kernel.Storage.InMem.Management.API (InMemManagementAPI)
import qualified Kernel.Storage.InMem.Management.Handler as Handler
import Kernel.Types.App (EnvR (..), FlowServerR)
import Kernel.Types.Flow (FlowR)
import Kernel.Utils.Error.FlowHandling (apiHandler, withFlowHandler)
import Kernel.Utils.Servant.Server (run)
import Network.Wai (pathInfo)
import Servant

type API = "internal" :> InMemManagementAPI

-- | Serves the in-memory cache management API (used by the shudhi sidecar) on the
-- scheduler's health server port. Requests run against the allocator's own
-- 'HandlerEnv', so they see the same in-memory cache the jobs use.
withInMemManagement :: R.FlowRuntime -> HandlerEnv -> Application -> Application
withInMemManagement flowRt env healthApp req sendResponse =
  case pathInfo req of
    "internal" : "inMem" : _ -> inMemApp req sendResponse
    _ -> healthApp req sendResponse
  where
    inMemApp = run (Proxy @API) handler EmptyContext (EnvR flowRt env)

handler :: FlowServerR HandlerEnv API
handler mbToken =
  (\mbPattern limit offset -> withHandler $ Handler.getKeys mbToken mbPattern limit offset)
    :<|> (withHandler . Handler.getValue mbToken)
    :<|> (withHandler . Handler.refreshCache mbToken)
    :<|> withHandler (Handler.getServerInfo mbToken)

-- HandlerEnv has no isShuttingDown, so withFlowHandlerAPI (which gates on it) doesn't fit.
withHandler :: FlowR HandlerEnv a -> ReaderT (EnvR HandlerEnv) IO a
withHandler = withFlowHandler . apiHandler
