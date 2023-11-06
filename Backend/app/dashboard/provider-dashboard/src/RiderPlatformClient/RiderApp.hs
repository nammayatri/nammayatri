{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module RiderPlatformClient.RiderApp
  ( callRiderAppExotelApi,
  )
where

import qualified "rider-app" API.Dashboard as BAP
import qualified Dashboard.Common.Exotel as Exotel
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Utils.Common hiding (callAPI)
import Tools.Client

newtype ExotelAPIs = ExotelAPIs
  { exotelHeartbeat :: Exotel.ExotelHeartbeatReq -> Euler.EulerClient APISuccess
  }

mkRiderAppExotelAPIs :: Text -> ExotelAPIs
mkRiderAppExotelAPIs token = do
  ExotelAPIs {..}
  where
    exotelHeartbeat = Euler.client (Proxy :: Proxy BAP.ExotelAPI) token

callRiderAppExotelApi ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI ExotelAPIs m r b c
  ) =>
  (ExotelAPIs -> b) ->
  c
callRiderAppExotelApi = callServerAPI @_ @m @r APP_BACKEND_MANAGEMENT mkRiderAppExotelAPIs "callRiderAppExotelApi"
