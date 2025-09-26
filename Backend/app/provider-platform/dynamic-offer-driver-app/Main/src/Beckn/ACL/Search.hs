{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Search where

import qualified Beckn.OnDemand.Transformer.Search as TSearch
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Domain.Action.Beckn.Search as DSearch
import Kernel.Prelude
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common

buildSearchReqV2 ::
  (HasFlowEnv m r '["_version" ::: Text], CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  Subscriber.Subscriber ->
  Search.SearchReqV2 ->
  m DSearch.DSearchReq
buildSearchReqV2 subscriber req = do
  let context = req.searchReqContext
  messageId <- Utils.getMessageId context
  let message = req.searchReqMessage
  TSearch.buildSearchReq messageId subscriber message context
