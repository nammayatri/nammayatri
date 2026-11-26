{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The daily clock of one city's shared-run engine: turns the city's timetables into runs, hands each to its bound
-- vehicle (SharedLogic.SharedRun.Materialise, in-process) and re-arms itself. Started once per city from the
-- dashboard's scheduler trigger (MaterialiseSharedRunsTrigger).
module SharedLogic.Allocator.Jobs.SharedRun.MaterialiseSharedRuns
  ( materialiseSharedRuns,
  )
where

import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.DriverPool.DriverPoolStateEvent (DriverPoolStateEnv)
import qualified SharedLogic.SharedRun.Materialise as Materialise
import qualified Storage.CachedQueries.Merchant as CQM

materialiseSharedRuns :: (Materialise.MaterialiseFlow m r c, DriverPoolStateEnv r) => Job 'MaterialiseSharedRuns -> m ExecutionResult
materialiseSharedRuns Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
  merchant <- CQM.findById jobData.merchantId >>= fromMaybeM (MerchantNotFound jobData.merchantId.getId)
  resp <-
    Materialise.materialiseRuns merchant $
      Materialise.MaterialiseReq
        { merchantId = jobData.merchantId,
          merchantOperatingCityId = jobData.merchantOperatingCityId,
          serviceDate = Nothing,
          lookaheadDays = jobData.lookaheadDays
        }
  logInfo $ "shared runs materialised for city " <> jobData.merchantOperatingCityId.getId <> ": " <> show resp
  now <- getCurrentTime
  pure $ ReSchedule (addUTCTime (fromIntegral (fromMaybe 86400 jobData.intervalSeconds)) now)
