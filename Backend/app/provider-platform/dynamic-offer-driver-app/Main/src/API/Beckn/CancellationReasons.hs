{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.CancellationReasons (API, handler) where

import qualified Beckn.ACL.CancellationReasons as ACL
import qualified Beckn.Types.Core.Taxi.API.CancellationReasons as API
import qualified Beckn.Types.Core.Taxi.CancellationReasons.Types as API
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> API.CancellationReasonsAPI

handler :: FlowServer API
handler = sendCancellationReasons

sendCancellationReasons ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  API.CancellationReasonsReq ->
  FlowHandler API.CancellationReasons
sendCancellationReasons merchantId (SignatureAuthResult _ subscriber) req = withFlowHandlerAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "getCancellationReasonsAPI" "Received get_cancellation_reasons API call."
    ACL.buildCancellationReasonsResp merchantId subscriber req
