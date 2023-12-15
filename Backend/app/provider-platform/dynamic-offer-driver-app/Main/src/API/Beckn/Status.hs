{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Status (API, handler) where

import qualified Beckn.ACL.OnStatus as ACL
import qualified Beckn.ACL.Status as ACL
import qualified Beckn.Core as CallBAP
import qualified Beckn.Types.Core.Taxi.API.OnStatus as OnStatus
import qualified Beckn.Types.Core.Taxi.API.Status as Status
import qualified Domain.Action.Beckn.Status as DStatus
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> Status.StatusAPI

handler :: FlowServer API
handler = status

status ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Status.StatusReq ->
  FlowHandler AckResponse
status transporterId (SignatureAuthResult _ subscriber) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "Status API Flow" "Reached"

    dStatusReq <- ACL.buildStatusReq subscriber req
    dStatusRes <- DStatus.handler transporterId dStatusReq

    let context = req.context
    void $
      CallBAP.withCallback dStatusRes.transporter Context.STATUS OnStatus.onStatusAPI context context.bap_uri $
        pure $ ACL.mkOnStatusMessage dStatusRes

    pure Ack
