{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.OnUpdate where

import Beckn.Types.Core.Taxi.OnUpdate (OnUpdateMessage, OnUpdateMessageV2)
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq)
import Kernel.Utils.Servant.JSONBS
import Servant (JSON, Post, ReqBody, (:>))

type OnUpdateReq = BecknCallbackReq OnUpdateMessage

type OnUpdateReqV2 = BecknCallbackReq OnUpdateMessageV2

type OnUpdateRes = AckResponse

type OnUpdateAPI =
  "on_update"
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] OnUpdateRes

onUpdateAPI :: Proxy OnUpdateAPI
onUpdateAPI = Proxy

type OnUpdateAPIV1 =
  "on_update"
    :> ReqBody '[JSON] OnUpdateReq
    :> Post '[JSON] OnUpdateRes

onUpdateAPIV1 :: Proxy OnUpdateAPIV1
onUpdateAPIV1 = Proxy

type OnUpdateAPIV2 =
  "on_update"
    :> ReqBody '[JSON] OnUpdateReqV2
    :> Post '[JSON] OnUpdateRes

onUpdateAPIV2 :: Proxy OnUpdateAPIV2
onUpdateAPIV2 = Proxy
