{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.OnStatus where

import Beckn.Types.Core.Taxi.OnStatus (OnStatusMessage)
import qualified BecknV2.OnDemand.Types as Spec
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq)
import Kernel.Utils.Servant.JSONBS
import Servant (JSON, Post, ReqBody, (:>))

type OnStatusReq = BecknCallbackReq OnStatusMessage

type OnStatusReqV2 = Spec.OnStatusReq

type OnStatusRes = AckResponse

type OnStatusAPI =
  "on_status"
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] OnStatusRes

onStatusAPI :: Proxy OnStatusAPI
onStatusAPI = Proxy

type OnStatusAPIV1 =
  "on_status"
    :> ReqBody '[JSON] OnStatusReq
    :> Post '[JSON] OnStatusRes

onStatusAPIV1 :: Proxy OnStatusAPIV1
onStatusAPIV1 = Proxy

type OnStatusAPIV2 =
  "on_status"
    :> ReqBody '[JSON] OnStatusReqV2
    :> Post '[JSON] OnStatusRes

onStatusAPIV2 :: Proxy OnStatusAPIV2
onStatusAPIV2 = Proxy
