{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.UnblockCustomer
  ( unblockCustomer,
  )
where

import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Utils.Common
import Lib.Scheduler
import qualified SharedLogic.JobScheduler as RJS
import qualified Storage.Queries.PersonExtra as QPExtra

unblockCustomer ::
  ( EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Job 'RJS.UnblockCustomer ->
  m ExecutionResult
unblockCustomer Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  let customerId = jobData.customerId
  void $ QPExtra.updatingBlockedStateWithUntil customerId Nothing False Nothing
  logInfo $ "Automatically unblocked customer, customerId: " <> customerId.getId
  return Complete
