{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.Route
  ( API,
    handler,
    callTripRoute,
    buildTransaction,
  )
where

import qualified "rider-app" API.Dashboard.Route as BAP
import qualified Dashboard.Common as Common
import qualified "rider-app" Domain.Action.Dashboard.Route as Routes
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "rider-app" Domain.Types.Ride as DP
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp as Client
import Servant
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "ride"
    :> ApiAuth 'APP_BACKEND 'CUSTOMERS 'TRIP_ROUTE
    :> BAP.TripRouteAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler = callTripRoute

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  BAP.RouteEndPoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.TripRouteAPI endpoint) (Just APP_BACKEND) (Just apiTokenInfo) Nothing Nothing

callTripRoute ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Id DP.Ride ->
  Routes.TripRouteReq ->
  FlowHandler Maps.GetRoutesResp
callTripRoute merchantShortId apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction BAP.TripRouteAPIEndPoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.route.tripRoute) rideId req
