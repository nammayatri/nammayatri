{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.Select where

import Beckn.Types.Core.Taxi.Select (SelectMessage, SelectMessageV2)
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq, BecknReqV2)
import Kernel.Utils.Servant.JSONBS
import Servant (JSON, Post, ReqBody, (:>))

type SelectReq = BecknReq SelectMessage

type SelectReqV2 = BecknReqV2 SelectMessageV2

type SelectRes = AckResponse

type SelectAPI =
  "select"
    -- :> ReqBody '[JSON] SelectReq
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] SelectRes

selectAPI :: Proxy SelectAPI
selectAPI = Proxy

type SelectAPIV1 =
  "select"
    :> ReqBody '[JSON] SelectReq
    :> Post '[JSON] SelectRes

selectAPIV1 :: Proxy SelectAPIV1
selectAPIV1 = Proxy

type SelectAPIV2 =
  "select"
    :> ReqBody '[JSON] SelectReqV2
    :> Post '[JSON] SelectRes

selectAPIV2 :: Proxy SelectAPIV2
selectAPIV2 = Proxy
