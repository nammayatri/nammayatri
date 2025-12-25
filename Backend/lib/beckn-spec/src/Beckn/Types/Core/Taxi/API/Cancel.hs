{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.Cancel where

import Beckn.Types.Core.Taxi.Cancel.Req
import qualified BecknV2.OnDemand.Types as Spec
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Kernel.Utils.Servant.JSONBS
import Servant (JSON, Post, ReqBody, (:>))

type CancelReq = BecknReq CancelMessage

type CancelReqV2 = Spec.CancelReq

type CancelRes = AckResponse

type CancelAPI =
  "cancel"
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] CancelRes

cancelAPI :: Proxy CancelAPI
cancelAPI = Proxy

type CancelAPIV1 =
  "cancel"
    :> ReqBody '[JSON] CancelReq
    :> Post '[JSON] CancelRes

cancelAPIV1 :: Proxy CancelAPIV1
cancelAPIV1 = Proxy

type CancelAPIV2 =
  "cancel"
    :> ReqBody '[JSON] CancelReqV2
    :> Post '[JSON] CancelRes

cancelAPIV2 :: Proxy CancelAPIV2
cancelAPIV2 = Proxy
