{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Cancel (API, handler) where

import qualified Beckn.ACL.Cancel as ACL
import qualified Beckn.Types.Core.Taxi.API.Cancel as API
import qualified Beckn.Types.Core.Taxi.API.Cancel as Cancel
import qualified Domain.Action.Beckn.Cancel as DCancel
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> SignatureAuth "Authorization"
    :> API.CancelAPI

handler :: FlowServer API
handler = cancel

cancel ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Cancel.CancelReq ->
  FlowHandler AckResponse
cancel transporterId subscriber req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "Cancel API Flow" "Reached"
    dCancelReq <- ACL.buildCancelReq req
    case dCancelReq of
      Left cancelReq -> do
        Redis.whenWithLockRedis (cancelLockKey cancelReq.bookingId.getId) 60 $ do
          (merchant, booking) <- DCancel.validateCancelRequest transporterId subscriber cancelReq
          fork ("cancelBooking:" <> cancelReq.bookingId.getId) $
            DCancel.cancel cancelReq merchant booking
      Right cancelSearchReq -> do
        searchTry <- DCancel.validateCancelSearchRequest transporterId subscriber cancelSearchReq
        fork ("cancelSearch:" <> cancelSearchReq.transactionId) $
          DCancel.cancelSearch transporterId cancelSearchReq searchTry
    return Ack

cancelLockKey :: Text -> Text
cancelLockKey id = "Driver:Cancel:BookingId-" <> id
