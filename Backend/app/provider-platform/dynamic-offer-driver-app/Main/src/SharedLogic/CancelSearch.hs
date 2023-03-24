{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CancelSearch
  ( lockSearchTry,
    incrementSearchTryLockCounter,
  )
where

import qualified Domain.Types.SearchTry as SS
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import Tools.Error

incrementSearchTryLockCounter ::
  ( HasCacheConfig r,
    HedisFlow m r
  ) =>
  Id SS.SearchTry ->
  m ()
incrementSearchTryLockCounter searchTryId = do
  let searchLockKey = mkSearchLockKey searchTryId
  _ <- Hedis.incr searchLockKey
  Hedis.expire searchLockKey 120
  searchCancelled <- fromMaybe False <$> Hedis.get (mkStartedCancelInfomKey searchTryId)
  if searchCancelled
    then throwError (InternalError "SEARCH_STEP_CANCELLED")
    else pure ()

lockSearchTry ::
  ( HasCacheConfig r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  Id SS.SearchTry ->
  m ()
lockSearchTry searchTryId = do
  let searchLockKey = mkSearchLockKey searchTryId
      startedCancelInfomKey = mkStartedCancelInfomKey searchTryId
  val <- Hedis.incr searchLockKey
  cancellingSearchTry <- fromMaybe False <$> Hedis.get startedCancelInfomKey
  Hedis.expire searchLockKey 120
  when (val > 1 && not cancellingSearchTry) $ throwError (InternalError "FAILED_TO_CANCEL_SEARCH_STEP")
  Hedis.setExp startedCancelInfomKey True 120

mkStartedCancelInfomKey :: Id SS.SearchTry -> Text
mkStartedCancelInfomKey searchTryId = "SearchTry:Cancelling:" <> searchTryId.getId

mkSearchLockKey :: Id SS.SearchTry -> Text
mkSearchLockKey searchTryId = "LockCounter:SearchTry:" <> searchTryId.getId
