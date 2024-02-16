{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.Status where

import Beckn.Types.Core.Taxi.Status (StatusMessage)
import qualified BecknV2.OnDemand.Types as Spec
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Kernel.Utils.Servant.JSONBS
import Servant (JSON, Post, ReqBody, (:>))

type StatusReq = BecknReq StatusMessage

type StatusReqV2 = Spec.StatusReq

type StatusRes = AckResponse

type StatusAPI =
  "status"
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] StatusRes

statusAPI :: Proxy StatusAPI
statusAPI = Proxy

type StatusAPIV1 =
  "status"
    :> ReqBody '[JSON] StatusReq
    :> Post '[JSON] StatusRes

statusAPIV1 :: Proxy StatusAPIV1
statusAPIV1 = Proxy

type StatusAPIV2 =
  "status"
    :> ReqBody '[JSON] StatusReqV2
    :> Post '[JSON] StatusRes

statusAPIV2 :: Proxy StatusAPIV2
statusAPIV2 = Proxy
