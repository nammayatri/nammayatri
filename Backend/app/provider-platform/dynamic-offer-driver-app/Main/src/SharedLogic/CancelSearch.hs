{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CancelSearch
  ( lockSearchRequest,
    incrementSearchRequestLockCounter,
  )
where

import qualified Domain.Types.SearchRequest as SR
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import Tools.Error

incrementSearchRequestLockCounter ::
  ( HasCacheConfig r,
    HedisFlow m r
  ) =>
  Id SR.SearchRequest ->
  m ()
incrementSearchRequestLockCounter searchRequestId = do
  let searchLockKey = mkSearchLockKey searchRequestId
  _ <- Hedis.incr searchLockKey
  Hedis.expire searchLockKey 120
  searchCancelled <- fromMaybe False <$> Hedis.get (mkStartedCancelInfomKey searchRequestId)
  when searchCancelled $ throwError (InternalError "SEARCH_REQUEST_CANCELLED")

lockSearchRequest ::
  ( HasCacheConfig r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  Id SR.SearchRequest ->
  m ()
lockSearchRequest searchRequestId = do
  let searchLockKey = mkSearchLockKey searchRequestId
      startedCancelInfomKey = mkStartedCancelInfomKey searchRequestId
  val <- Hedis.incr searchLockKey
  cancellingSearchReq <- fromMaybe False <$> Hedis.get startedCancelInfomKey
  Hedis.expire searchLockKey 120
  when (val > 1 && not cancellingSearchReq) $ throwError (InternalError "FAILED_TO_CANCEL_SEARCH_REQUEST")
  Hedis.setExp startedCancelInfomKey True 120

mkStartedCancelInfomKey :: Id SR.SearchRequest -> Text
mkStartedCancelInfomKey searchRequestId = "SearchTry:Cancelling:" <> searchRequestId.getId

mkSearchLockKey :: Id SR.SearchRequest -> Text
mkSearchLockKey searchRequestId = "LockCounter:SearchRequest:" <> searchRequestId.getId
