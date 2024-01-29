{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.Update where

import Beckn.Types.Core.Taxi.Update
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Kernel.Utils.Servant.JSONBS
import Servant (JSON, Post, ReqBody, (:>))

type UpdateReq = BecknReq UpdateMessage

type UpdateRes = AckResponse

type UpdateAPI =
  "update"
    -- :> ReqBody '[JSON] UpdateReq
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] UpdateRes

type UpdateAPIV1 =
  "update"
    :> ReqBody '[JSON] UpdateReq
    :> Post '[JSON] UpdateRes

type UpdateAPIV2 =
  "update"
    :> ReqBody '[JSON] UpdateReq
    :> Post '[JSON] UpdateRes

updateAPI :: Proxy UpdateAPI
updateAPI = Proxy

updateAPIV1 :: Proxy UpdateAPIV1
updateAPIV1 = Proxy

updateAPIV2 :: Proxy UpdateAPIV2
updateAPIV2 = Proxy
