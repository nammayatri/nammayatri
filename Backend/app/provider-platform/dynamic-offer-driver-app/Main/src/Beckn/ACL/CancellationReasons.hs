{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.CancellationReasons where

import Beckn.Types.Core.Taxi.CancellationReasons.Types
import Domain.Action.Beckn.CancellationReason
import Domain.Types.Merchant (Merchant)
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import SharedLogic.Merchant
import Storage.CachedQueries.CacheConfig (CacheFlow)
import Tools.Error

buildCancellationReasonsResp ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    Esq.EsqDBReplicaFlow m r,
    CacheFlow m r,
    Esq.EsqDBFlow m r
  ) =>
  Id Merchant ->
  Subscriber.Subscriber ->
  CancellationReasonsReq ->
  m CancellationReasons
buildCancellationReasonsResp merchantId subscriber req = do
  let ctx = req.context
  validateContext Context.GET_CANCELLATION_REASONS ctx
  unless (subscriber.subscriber_id == ctx.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == ctx.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  checkMerchantExist merchantId
  getCancellationReasons
