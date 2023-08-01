{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.UnblockDriverUpdate.UnblockDriver
  ( unblockDriver,
  )
where

import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.GoogleTranslate (TranslateFlow)
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import qualified Storage.CachedQueries.DriverInformation as CQDriverInfo
import qualified Tools.Metrics as Metrics

unblockDriver ::
  ( TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r,
    Log m
  ) =>
  Job 'UnblockDriver ->
  m ExecutionResult
unblockDriver Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  let driverId = jobData.driverId
  CQDriverInfo.updateBlockedState (cast driverId) False
  return Complete
