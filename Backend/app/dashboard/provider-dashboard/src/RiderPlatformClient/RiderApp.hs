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
    callRiderAppBapFlowDebug,
  )
where

import qualified "rider-app" API.Dashboard as BAP
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Ride as ProviderRide
import qualified Dashboard.Common.Exotel as Exotel
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common hiding (callAPI)
import Servant
import qualified "rider-app" Tools.Auth as RiderAuth
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

-- Standalone API type for the BAP flow debug endpoint on rider-app.
-- This must match the route structure in rider-app's Dashboard API.
type BAPFlowDebugAPI =
  "dashboard"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> Capture "city" City.City
    :> RiderAuth.DashboardTokenAuth
    :> "ride"
    :> "flowDebug"
    :> "bap"
    :> QueryParam "bapBookingId" Text
    :> QueryParam "bapRideId" Text
    :> QueryParam "bapRideShortId" Text
    :> QueryParam "bapSearchRequestId" Text
    :> QueryParam "bppBookingId" Text
    :> QueryParam "transactionId" Text
    :> Get '[JSON] ProviderRide.BAPSideDebug

newtype BAPFlowDebugAPIs = BAPFlowDebugAPIs
  { getBapFlowDebug :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Euler.EulerClient ProviderRide.BAPSideDebug
  }

mkBAPFlowDebugAPIs :: ShortId DM.Merchant -> City.City -> Text -> BAPFlowDebugAPIs
mkBAPFlowDebugAPIs merchantId city token =
  BAPFlowDebugAPIs {..}
  where
    getBapFlowDebug = Euler.client (Proxy :: Proxy BAPFlowDebugAPI) merchantId city token

callRiderAppBapFlowDebug ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI BAPFlowDebugAPIs m r b c
  ) =>
  ShortId DM.Merchant ->
  City.City ->
  (BAPFlowDebugAPIs -> b) ->
  c
callRiderAppBapFlowDebug merchantId city =
  callServerAPI @_ @m @r APP_BACKEND_MANAGEMENT (mkBAPFlowDebugAPIs merchantId city) "callRiderAppBapFlowDebug"
