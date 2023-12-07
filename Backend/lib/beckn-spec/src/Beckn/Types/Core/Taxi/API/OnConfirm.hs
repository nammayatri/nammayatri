{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.OnConfirm where

import Beckn.Types.Core.Taxi.OnConfirm (OnConfirmMessage, OnConfirmMessageV2)
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq)
import Servant (JSON, Post, ReqBody, (:>))

type OnConfirmReq = BecknCallbackReq OnConfirmMessage

type OnConfirmReqV2 = BecknCallbackReq OnConfirmMessageV2

type OnConfirmRes = AckResponse

type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] OnConfirmReq
    :> Post '[JSON] OnConfirmRes

type OnConfirmAPIV2 =
  "on_confirm"
    :> ReqBody '[JSON] OnConfirmReqV2
    :> Post '[JSON] OnConfirmRes

onConfirmAPI :: Proxy OnConfirmAPI
onConfirmAPI = Proxy

onConfirmAPIV2 :: Proxy OnConfirmAPIV2
onConfirmAPIV2 = Proxy
