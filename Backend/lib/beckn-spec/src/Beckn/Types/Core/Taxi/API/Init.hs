{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.Init where

import Beckn.Types.Core.Taxi.Init
import qualified BecknV2.OnDemand.Types as Spec
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Kernel.Utils.Servant.JSONBS
import Servant (JSON, Post, ReqBody, (:>))

type InitReq = BecknReq InitMessage

type InitReqV2 = Spec.InitReq

type InitRes = AckResponse

type InitAPI =
  "init"
    -- :> ReqBody '[JSON] InitReq
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] InitRes

type InitAPIV1 =
  "init"
    :> ReqBody '[JSON] InitReq
    :> Post '[JSON] InitRes

type InitAPIV2 =
  "init"
    :> ReqBody '[JSON] InitReqV2
    :> Post '[JSON] InitRes

initAPI :: Proxy InitAPI
initAPI = Proxy

initAPIV1 :: Proxy InitAPIV1
initAPIV1 = Proxy

initAPIV2 :: Proxy InitAPIV2
initAPIV2 = Proxy
