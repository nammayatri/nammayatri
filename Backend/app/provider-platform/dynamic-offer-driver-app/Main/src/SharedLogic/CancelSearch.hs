{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CancelSearch
  ( lockSearchStep,
    incrementSearchStepLockCounter,
  )
where

import qualified Domain.Types.SearchStep as SS
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import Tools.Error

incrementSearchStepLockCounter ::
  ( HasCacheConfig r,
    HedisFlow m r
  ) =>
  Id SS.SearchStep ->
  m ()
incrementSearchStepLockCounter searchStepId = do
  let searchLockKey = mkSearchLockKey searchStepId
  _ <- Hedis.incr searchLockKey
  Hedis.expire searchLockKey 120
  searchCancelled <- fromMaybe False <$> Hedis.get (mkStartedCancelInfomKey searchStepId)
  if searchCancelled
    then throwError (InternalError "SEARCH_STEP_CANCELLED")
    else pure ()

lockSearchStep ::
  ( HasCacheConfig r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  Id SS.SearchStep ->
  m ()
lockSearchStep searchStepId = do
  let searchLockKey = mkSearchLockKey searchStepId
      startedCancelInfomKey = mkStartedCancelInfomKey searchStepId
  val <- Hedis.incr searchLockKey
  cancellingSearchStep <- fromMaybe False <$> Hedis.get startedCancelInfomKey
  Hedis.expire searchLockKey 120
  when (val > 1 && not cancellingSearchStep) $ throwError (InternalError "FAILED_TO_CANCEL_SEARCH_STEP")
  Hedis.setExp startedCancelInfomKey True 120

mkStartedCancelInfomKey :: Id SS.SearchStep -> Text
mkStartedCancelInfomKey searchStepId = "SearchStep:Cancelling:" <> searchStepId.getId

mkSearchLockKey :: Id SS.SearchStep -> Text
mkSearchLockKey searchStepId = "LockCounter:SearchStep:" <> searchStepId.getId
