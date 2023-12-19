{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.Search where

import Beckn.Types.Core.Taxi.Search (SearchMessage, SearchMessageV2)
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq, BecknReqV2)
import Kernel.Utils.Servant.JSONBS
import Servant (JSON, Post, ReqBody, (:>))

type SearchReq = BecknReq SearchMessage

type SearchReqV2 = BecknReqV2 SearchMessageV2

type SearchRes = AckResponse

type SearchAPI =
  "search"
    -- :> ReqBody '[JSON] SearchReq
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] SearchRes

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

type SearchAPIV1 =
  "search"
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] SearchRes

searchAPIV1 :: Proxy SearchAPIV1
searchAPIV1 = Proxy

type SearchAPIV2 =
  "search"
    :> ReqBody '[JSON] SearchReqV2
    :> Post '[JSON] SearchRes

searchAPIV2 :: Proxy SearchAPIV2
searchAPIV2 = Proxy
