{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.OnSearch where

import Beckn.Types.Core.Taxi.OnSearch
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq)
import Servant (JSON, Post, ReqBody, (:>))

type OnSearchReq = BecknCallbackReq OnSearchMessage

type OnSearchReqV2 = BecknCallbackReq OnSearchMessageV2

type OnSearchRes = AckResponse

type OnSearchAPI =
  "on_search"
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] OnSearchRes

type OnSearchAPIV2 =
  "on_search"
    :> ReqBody '[JSON] OnSearchReqV2
    :> Post '[JSON] OnSearchRes

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy

onSearchAPIV2 :: Proxy OnSearchAPIV2
onSearchAPIV2 = Proxy
