{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.Search where

import Beckn.Types.Core.Taxi.Search (SearchMessage)
import qualified BecknV2.OnDemand.Types as Spec
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Servant (Capture, Header, JSON, Post, QueryParam, ReqBody, (:>))

type SearchReq = BecknReq SearchMessage

type SearchReqV2 = Spec.SearchReq

type SearchRes = AckResponse

type SearchAPI =
  "search"
    :> ReqBody '[JSON] SearchReqV2
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

type SyncSearchRes = Spec.OnSearchReq

-- | isShadowSearch marks a request the BAP is making to price a better-route-point
-- suggestion alongside a real search. The BPP still persists a SearchRequest and Estimates
-- for it (the customer can select them), but suppresses the search/estimate events, namma
-- tags and demand-hotspot updates that would otherwise be counted twice for one customer
-- intent. Absent or False means an ordinary search.
type SyncSearchAPI =
  "sync_search"
    :> Capture "merchantId" Text
    :> Header "token" Text
    :> QueryParam "isShadowSearch" Bool
    -- parentTransactionId: the search a shadow belongs to. Lets the provider price both
    -- against one set of dynamic-pricing inputs, so a suggestion is not charged congestion
    -- the customer's own search escaped. Plain comment, not Haddock: a `-- |` between two
    -- `:>` operators is a parse error in the Haddock-mode parser ormolu runs.
    :> QueryParam "parentTransactionId" Text
    :> ReqBody '[JSON] SearchReqV2
    :> Post '[JSON] SyncSearchRes

syncSearchAPI :: Proxy SyncSearchAPI
syncSearchAPI = Proxy
