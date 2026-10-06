{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | In-memory cache management API (@/internal/inMem/*@, used by the shudhi sidecar)
-- for services that don't run a full servant app: schedulers and kafka consumers.
-- Requests run against the given env, so they see the same in-memory cache the
-- service's own flows use.
module Lib.Scheduler.InMemManagement
  ( InMemManagementEnv,
    inMemManagementApp,
    withInMemManagement,
  )
where

import qualified EulerHS.Runtime as R
import Kernel.Prelude
import Kernel.Storage.InMem.Management.API (InMemManagementAPI)
import qualified Kernel.Storage.InMem.Management.Handler as Handler
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.App (EnvR (..), FlowHandlerR, FlowServerR)
import Kernel.Types.Flow (FlowR, HasFlowHandlerR)
import Kernel.Utils.Error.FlowHandling (apiHandler, withFlowHandler)
import Kernel.Utils.Servant.Server (run)
import Network.Wai (pathInfo)
import Servant

type API = "internal" :> InMemManagementAPI

type InMemManagementEnv r =
  ( HasFlowHandlerR (FlowR r) r,
    Metrics.CoreMetrics (FlowR r),
    HasField "url" r (Maybe Text)
  )

-- | Serves only the in-memory cache management API.
inMemManagementApp :: forall r. InMemManagementEnv r => R.FlowRuntime -> r -> Application
inMemManagementApp flowRt env = run (Proxy @API) handler EmptyContext (EnvR flowRt env)

-- | Routes @/internal/inMem/*@ to 'inMemManagementApp' and everything else to the wrapped app.
withInMemManagement :: InMemManagementEnv r => R.FlowRuntime -> r -> Application -> Application
withInMemManagement flowRt env app req respond =
  case pathInfo req of
    "internal" : "inMem" : _ -> inMemApp req respond
    _ -> app req respond
  where
    inMemApp = inMemManagementApp flowRt env

handler :: InMemManagementEnv r => FlowServerR r API
handler mbToken =
  (\mbPattern limit offset -> withHandler $ Handler.getKeys mbToken mbPattern limit offset)
    :<|> (withHandler . Handler.getValue mbToken)
    :<|> (withHandler . Handler.refreshCache mbToken)
    :<|> withHandler (Handler.getServerInfo mbToken)

-- Not withFlowHandlerAPI: scheduler envs have no isShuttingDown to gate on.
withHandler :: InMemManagementEnv r => FlowR r a -> FlowHandlerR r a
withHandler = withFlowHandler . apiHandler
