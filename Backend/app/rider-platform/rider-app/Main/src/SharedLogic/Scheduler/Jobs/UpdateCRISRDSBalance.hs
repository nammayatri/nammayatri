{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.UpdateCRISRDSBalance where

import qualified Data.Text as T
import qualified Data.Time as DT
import qualified Domain.Types.CrisRdsBalanceHistory as DCRBH
import Domain.Types.IntegratedBPPConfig
import ExternalBPP.ExternalAPI.Subway.CRIS.UpdateRDSBalance (getRDSBalance)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
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
updateCRISRDSBalanceJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  executionTime <- getCurrentTime

  let jobData = jobInfo.jobData
      integratedBppConfigId = jobData.integratedBPPConfigId
      istOffset = 19800 :: DT.NominalDiffTime -- IST offset: 5 hours 30 minutes
  integratedBppConfig <- QIntegratedBPPConfig.findById integratedBppConfigId >>= fromMaybeM (InvalidRequest $ "integratedBppConfig not found for id: " <> show integratedBppConfigId)
  case integratedBppConfig.providerConfig of
    CRIS config@CRISConfig {..} -> do
      let currentISTTime = DT.addUTCTime istOffset executionTime
          currentDay = DT.utctDay currentISTTime

      -- Check if execution is within 5-min buffer of scheduled time
      (finalDateIst, nextJobDate) <- case balanceCheckTimeOfDay of
        Just scheduleTimeOfDay -> do
          let scheduledTimeOfDay = fromIntegral scheduleTimeOfDay :: DT.DiffTime
              currentTimeOfDay = DT.utctDayTime currentISTTime

              -- Determine when job should have run: today or yesterday?
              (scheduledDay, scheduledISTTime) =
                if currentTimeOfDay >= scheduledTimeOfDay
                  then (currentDay, DT.UTCTime currentDay scheduledTimeOfDay)
                  else (DT.addDays (-1) currentDay, DT.UTCTime (DT.addDays (-1) currentDay) scheduledTimeOfDay)

              scheduledUTCTime = DT.addUTCTime (negate istOffset) scheduledISTTime
              timeDiff = diffUTCTime executionTime scheduledUTCTime
              bufferSeconds = 300 :: DT.NominalDiffTime -- 5 minutes
              withinBuffer = timeDiff >= 0 && timeDiff <= bufferSeconds

          if withinBuffer
            then do
              let scheduledDateIst = T.pack $ DT.formatTime DT.defaultTimeLocale "%Y-%m-%d" scheduledDay
              logInfo $ "Within buffer (" <> show (floor timeDiff :: Integer) <> "s), using scheduled date: " <> scheduledDateIst
              pure (Just scheduledDateIst, DT.addDays 1 scheduledDay)
            else do
              logWarning $ "Outside buffer (" <> show (floor timeDiff :: Integer) <> "s), recording without date for monitoring only"
              -- If scheduled time already passed today, schedule for tomorrow; otherwise today
              let nextJobDate =
                    if currentTimeOfDay >= scheduledTimeOfDay
                      then DT.addDays 1 currentDay
                      else currentDay
              pure (Nothing, nextJobDate)
        Nothing -> do
          -- No scheduled time configured, just use current date
          logInfo "No scheduled time configured, using current date"
          pure (Just (T.pack $ DT.formatTime DT.defaultTimeLocale "%Y-%m-%d" currentDay), DT.addDays 1 currentDay)

      -- Check if we already have a record for this date+config (idempotency)
      existingRecord <- case finalDateIst of
        Just dateIst -> QCrisRdsBalanceHistory.findByDateAndConfig (Just dateIst) integratedBppConfigId
        Nothing -> pure Nothing
      result <-
        withTryCatch "updateCRISRDSBalanceJob:recordBalance" $ case existingRecord of
          Nothing -> do
            balanceResp <- getRDSBalance config
            now <- getCurrentTime
            balanceHistoryId <- generateGUID
            let balanceHistory =
                  DCRBH.CrisRdsBalanceHistory
                    { id = Id balanceHistoryId,
                      integratedBppConfigId = integratedBppConfigId,
                      balance = balanceResp.balance,
                      executionTime = executionTime,
                      dateIst = finalDateIst,
                      createdAt = now,
                      updatedAt = now
                    }
            QCrisRdsBalanceHistory.create balanceHistory
            case finalDateIst of
              Just dateIst -> logInfo $ "Successfully recorded balance for date: " <> dateIst
              Nothing -> logInfo "Successfully recorded balance for monitoring (no target date)"
          Just _ -> do
            logInfo $ "Balance record already exists for date: " <> fromMaybe "monitoring" finalDateIst <> ", skipping creation"

      case result of
        Left err -> logError $ "Failed to record balance: " <> show err
        Right _ -> pure ()

      -- Schedule next job
      whenJust balanceCheckTimeOfDay $ \scheduleTimeOfDay -> do
        now <- getCurrentTime
        let targetTimeOfDay = fromIntegral scheduleTimeOfDay
            nextTargetISTTime = DT.UTCTime nextJobDate targetTimeOfDay
            nextTargetUTCTime = DT.addUTCTime (negate istOffset) nextTargetISTTime
            scheduleAfter = max 60 $ diffUTCTime nextTargetUTCTime now
            nextTargetDateIst = T.pack $ DT.formatTime DT.defaultTimeLocale "%Y-%m-%d" nextJobDate
            newJobData =
              UpdateCRISRDSBalanceJobData
                { integratedBPPConfigId = integratedBppConfigId
                }

        createJobIn @_ @'UpdateCRISRDSBalance (Just integratedBppConfig.merchantId) (Just integratedBppConfig.merchantOperatingCityId) scheduleAfter (newJobData :: UpdateCRISRDSBalanceJobData)
        logInfo $ "Scheduled next balance check for date: " <> nextTargetDateIst

      return Complete
    _ -> throwError $ InternalError $ "Incorrect bpp config with id " <> (show $ integratedBppConfig.id)
