{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.OnInit where

import Beckn.Types.Core.Taxi.OnInit
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq, BecknCallbackReqV2)
import Kernel.Utils.Servant.JSONBS
import Servant (JSON, Post, ReqBody, (:>))

type OnInitReq = BecknCallbackReq OnInitMessage

type OnInitReqV2 = BecknCallbackReqV2 OnInitMessageV2

type OnInitRes = AckResponse

type OnInitAPI =
  "on_init"
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] OnInitRes

type OnInitAPIV1 =
  "on_init"
    :> ReqBody '[JSON] OnInitReq
    :> Post '[JSON] OnInitRes

type OnInitAPIV2 =
  "on_init"
    :> ReqBody '[JSON] OnInitReqV2
    :> Post '[JSON] OnInitRes

onInitAPI :: Proxy OnInitAPI
onInitAPI = Proxy

onInitAPIV1 :: Proxy OnInitAPIV1
onInitAPIV1 = Proxy

onInitAPIV2 :: Proxy OnInitAPIV2
onInitAPIV2 = Proxy
