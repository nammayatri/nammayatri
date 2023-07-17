{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.SyncBookingStatus
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.SyncBookingStatus as Refresh
import qualified Domain.Action.UI.SyncBookingStatus as SyncBooking
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as SP
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import Tools.Auth (TokenAuth)

type API =
  "sync"
    :> "booking"
    :> "status"
    :> TokenAuth
    :> ReqBody '[JSON] SyncBooking.SyncBookingStatusReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = syncBookingStatus

syncBookingStatus :: (Id SP.Person, Id Merchant.Merchant) -> SyncBooking.SyncBookingStatusReq -> FlowHandler APISuccess
syncBookingStatus (personId, merchantId) req = withFlowHandlerAPI . withPersonIdLogTag personId $ Refresh.syncBookingStatus personId merchantId req
