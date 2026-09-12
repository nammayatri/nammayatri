{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The operator-action audit trail, at the path provider-dashboard serves it on.
--
-- The @endpoint@ query parameter is plain text here rather than the parsed
-- endpoint enum. That is not a contract change: lib-dashboard-api's
-- @ToHttpApiData Endpoint@ is @toUrlPiece = show@, so the value on the wire was
-- always this string.
module API.Dashboard.TransactionView
  ( API,
    handler,
  )
where

import qualified Domain.Action.Dashboard.TransactionView as DTransaction
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransactionView as DT
import Kernel.Prelude
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Id
import Kernel.Utils.Common (FlowHandlerR, FlowServerR)
import Servant
import Tools.Auth.Dashboard
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow, withDashboardDbFlowHandlerAPI)

type API =
  "listTransactions"
    :> DashboardAuth 'DASHBOARD_USER
    :> QueryParam "searchString" Text
    :> QueryParam "limit" Integer
    :> QueryParam "offset" Integer
    :> QueryParam "requestorId" (Id DP.Person)
    :> QueryParam "driverId" Text
    :> QueryParam "rideId" Text
    :> QueryParam "endpoint" Text
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> Get '[JSON] DT.ListTransactionRes

handler :: DashboardLoginFlow (FlowR r) r => FlowServerR r API
handler = listTransactions

listTransactions :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Maybe Text -> Maybe Integer -> Maybe Integer -> Maybe (Id DP.Person) -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> FlowHandlerR r DT.ListTransactionRes
listTransactions tokenInfo mbSearchString mbLimit mbOffset mbRequestorId mbDriverId mbRideId mbEndpoint mbFrom mbTo =
  withDashboardDbFlowHandlerAPI $
    DTransaction.listTransactions tokenInfo mbSearchString mbLimit mbOffset mbRequestorId mbDriverId mbRideId mbEndpoint mbFrom mbTo
