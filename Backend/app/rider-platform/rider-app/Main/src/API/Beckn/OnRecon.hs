{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnRecon (API, handler) where

import qualified Beckn.ACL.OnRecon as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.Recon as OnRecon
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Data.Aeson as A
import qualified Domain.Action.PPF.Recon as DRecon
import Environment
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import TransactionLogs.PushLogs

type API = OnRecon.OnReconAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onRecon

onRecon ::
  SignatureAuthResult ->
  OnRecon.OnReconReqV2 ->
  FlowHandler AckResponse
onRecon _ reqV2 = withFlowHandlerBecknAPI do
  transactionId <- Utils.getTransactionId reqV2.onReconReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logInfo "Received on_recon callback from BPP"
    domainReq <- ACL.buildOnReconDomainReq reqV2
    fork "on_recon processing" $
      DRecon.processOnRecon domainReq
    fork "on_recon received pushing ondc logs" do
      bppId <- Utils.getContextBppId reqV2.onReconReqContext
      void $ pushLogs "on_recon" (A.toJSON reqV2) bppId "MOBILITY"
    pure Ack
