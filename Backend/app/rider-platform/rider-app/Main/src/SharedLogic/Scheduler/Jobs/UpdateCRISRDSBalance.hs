{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.UpdateCRISRDSBalance where

import qualified Data.Time as DT
import qualified Domain.Types.CrisRdsBalanceHistory as DCRBH
import Domain.Types.IntegratedBPPConfig
import ExternalBPP.ExternalAPI.Subway.CRIS.UpdateRDSBalance (getRDSBalance)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobInWithCheck)
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.CrisRdsBalanceHistory as QCrisRdsBalanceHistory
import qualified Storage.Queries.IntegratedBPPConfig as QIntegratedBPPConfig

updateCRISRDSBalanceJob ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'UpdateCRISRDSBalance ->
  m ExecutionResult
updateCRISRDSBalanceJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ withLogTag "UpdateCRISRDSBalance" do
  executionTime <- getCurrentTime

  let jobData = jobInfo.jobData
      integratedBppConfigId = jobData.integratedBPPConfigId
      istOffset = 19800 :: DT.NominalDiffTime
  integratedBppConfig <- QIntegratedBPPConfig.findById integratedBppConfigId >>= fromMaybeM (InvalidRequest $ "integratedBppConfig not found for id: " <> show integratedBppConfigId)
  case integratedBppConfig.providerConfig of
    CRIS config@CRISConfig {..} -> do
      let currentISTTime = DT.addUTCTime istOffset executionTime
          currentDay = DT.utctDay currentISTTime

      let (targetDateIst, nextJobDate) = case balanceCheckTimeOfDay of
            Just scheduleTimeOfDay ->
              let scheduledTimeOfDay = fromIntegral scheduleTimeOfDay :: DT.DiffTime
                  currentTimeOfDay = DT.utctDayTime currentISTTime
                  scheduledDay =
                    if currentTimeOfDay >= scheduledTimeOfDay
                      then currentDay
                      else DT.addDays (-1) currentDay
                  scheduledISTTime = DT.UTCTime scheduledDay scheduledTimeOfDay
                  scheduledUTCTime = DT.addUTCTime (negate istOffset) scheduledISTTime
                  timeDiff = diffUTCTime executionTime scheduledUTCTime
                  bufferSeconds = 7200 :: DT.NominalDiffTime
                  dateIst =
                    if timeDiff >= 0 && timeDiff <= bufferSeconds
                      then scheduledDay
                      else currentDay
                  nextDate =
                    if currentTimeOfDay >= scheduledTimeOfDay
                      then DT.addDays 1 currentDay
                      else currentDay
               in (dateIst, nextDate)
            Nothing -> (currentDay, DT.addDays 1 currentDay)

      let scheduledUTCForTarget = case balanceCheckTimeOfDay of
            Just scheduleTimeOfDay ->
              let scheduledTimeOfDay = fromIntegral scheduleTimeOfDay :: DT.DiffTime
                  scheduledISTTime = DT.UTCTime targetDateIst scheduledTimeOfDay
               in DT.addUTCTime (negate istOffset) scheduledISTTime
            Nothing -> executionTime

      let lockKey = "CRISRDSBalance:Lock:" <> show integratedBppConfigId <> ":" <> show targetDateIst
      isLockAcquired <- Hedis.tryLockRedis lockKey 300
      if isLockAcquired
        then do
          existingRecords <- QCrisRdsBalanceHistory.findByDateAndConfig (Just targetDateIst) integratedBppConfigId
          result <-
            withTryCatch "updateCRISRDSBalanceJob:recordBalance" $ case existingRecords of
              [] -> do
                balanceResp <- getRDSBalance config
                now <- getCurrentTime
                balanceHistoryId <- generateGUID
                let balanceHistory =
                      DCRBH.CrisRdsBalanceHistory
                        { id = Id balanceHistoryId,
                          integratedBppConfigId = integratedBppConfigId,
                          balance = balanceResp.balance,
                          executionTime = executionTime,
                          dateIst = Just targetDateIst,
                          createdAt = now,
                          updatedAt = now
                        }
                QCrisRdsBalanceHistory.create balanceHistory
                logInfo $ "Successfully recorded balance for date: " <> show targetDateIst
              (existing : _) -> do
                let existingDist = abs $ diffUTCTime existing.executionTime scheduledUTCForTarget
                    currentDist = abs $ diffUTCTime executionTime scheduledUTCForTarget
                if currentDist < existingDist
                  then do
                    balanceResp <- getRDSBalance config
                    QCrisRdsBalanceHistory.updateBalanceAndExecutionTimeByDateAndConfig balanceResp.balance executionTime (Just targetDateIst) integratedBppConfigId
                    logInfo $ "Updated balance for date: " <> show targetDateIst <> " (closer to scheduled time: " <> show (floor currentDist :: Integer) <> "s vs " <> show (floor existingDist :: Integer) <> "s)"
                  else logInfo $ "Balance record already exists for date: " <> show targetDateIst <> " with closer execution time, skipping"
          case result of
            Left err -> logError $ "Failed to record balance: " <> show err
            Right _ -> pure ()
          whenJust balanceCheckTimeOfDay $ \scheduleTimeOfDay -> do
            now <- getCurrentTime
            let targetTimeOfDay = fromIntegral scheduleTimeOfDay
                nextTargetISTTime = DT.UTCTime nextJobDate targetTimeOfDay
                nextTargetUTCTime = DT.addUTCTime (negate istOffset) nextTargetISTTime
                scheduleAfter = max 60 $ diffUTCTime nextTargetUTCTime now
                minScheduleTime = DT.addUTCTime (-3600) nextTargetUTCTime
                maxScheduleTime = DT.addUTCTime 3600 nextTargetUTCTime
                newJobData =
                  UpdateCRISRDSBalanceJobData
                    { integratedBPPConfigId = integratedBppConfigId
                    }

            createJobInWithCheck @_ @'UpdateCRISRDSBalance (Just integratedBppConfig.merchantId) (Just integratedBppConfig.merchantOperatingCityId) scheduleAfter minScheduleTime maxScheduleTime (show UpdateCRISRDSBalance) (Just 1) (newJobData :: UpdateCRISRDSBalanceJobData)
            logInfo $ "Scheduled next balance check for date: " <> show nextJobDate
        else logInfo $ "Could not acquire lock for date: " <> show targetDateIst <> ", another instance is handling it"

      return Complete
    _ -> throwError $ InternalError $ "Incorrect bpp config with id " <> (show $ integratedBppConfig.id)
