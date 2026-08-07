{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module BecknV2.RSF.APIs where

import qualified BecknV2.RSF.Types as Spec
import EulerHS.Prelude
import Kernel.Utils.Servant.JSONBS
import Servant (JSON, Post, ReqBody, (:>))

type ReceiverReconAPI =
  "receiver_recon"
    :> ReqBody '[JSON] Spec.ReceiverReconReq
    :> Post '[JSON] Spec.RSFAckResponse

receiverReconAPI :: Proxy ReceiverReconAPI
receiverReconAPI = Proxy

type ReceiverReconAPIBS =
  "receiver_recon"
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] Spec.RSFAckResponse

receiverReconAPIBS :: Proxy ReceiverReconAPIBS
receiverReconAPIBS = Proxy

type OnReceiverReconAPI =
  "on_receiver_recon"
    :> ReqBody '[JSON] Spec.OnReceiverReconReq
    :> Post '[JSON] Spec.RSFAckResponse

onReceiverReconAPI :: Proxy OnReceiverReconAPI
onReceiverReconAPI = Proxy
