{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Client for an application server's Exotel heartbeat route.
--
-- The Exotel heartbeat fans out to both application servers. Whichever server
-- hosts the endpoint reaches the other one through this; the route it targets
-- (@\/exotel\/heartbeat@ behind the shared dashboard token) is identical on both.
module Tools.ExotelClient
  ( callExotelHeartbeat,
    ExotelAPIs (..),
  )
where

import qualified Dashboard.Common.Exotel as Exotel
import qualified Domain.Types.ServerName as DSN
import qualified EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Utils.Common
import Servant
import Tools.Client
import Tools.Metrics

type ExotelAPI =
  Header "token" Text
    :> "exotel"
    :> Exotel.ExotelHeartbeatAPI

newtype ExotelAPIs = ExotelAPIs
  { exotelHeartbeat :: Exotel.ExotelHeartbeatReq -> Euler.EulerClient APISuccess
  }

mkExotelAPIs :: Text -> ExotelAPIs
mkExotelAPIs token = ExotelAPIs {exotelHeartbeat = Euler.client (Proxy :: Proxy ExotelAPI) (Just token)}

callExotelHeartbeat ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DSN.DataServer]],
    CallServerAPI ExotelAPIs m r b c
  ) =>
  DSN.ServerName ->
  (ExotelAPIs -> b) ->
  c
callExotelHeartbeat serverName = callServerAPI @_ @m @r serverName mkExotelAPIs "callExotelHeartbeat"
