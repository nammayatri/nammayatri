{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.CrisRecon where

import qualified Domain.Types.FRFSTicketBookingStatus as FRFSTicketBookingStatus
import qualified Domain.Types.IntegratedBPPConfig as DIntegratedBPPConfig
import ExternalBPP.ExternalAPI.Subway.CRIS.ReconTicketEnquiry (getReconTicketEnquiry)
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.IntegratedBPPConfig as QIntegratedBPPConfig

crisReconJob ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'CrisRecon ->
  m ExecutionResult
crisReconJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId' = jobData.merchantId
      merchantOperatingCityId' = jobData.merchantOperatingCityId
      checkSince = jobData.checkSince
      scheduleItself = jobData.scheduleItself

  now <- getCurrentTime
  let oneHourAgo = addUTCTime (intToNominalDiffTime (-3600 * checkSince)) now -- checking for last (checkSince hours)
  recentFailedBookings <- QFRFSTicketBooking.findAllByProviderNameAndCreatedAtAfterAndStatus "CRIS Subway" oneHourAgo FRFSTicketBookingStatus.FAILED

  logInfo $ "Found " <> show (length recentFailedBookings) <> " failed CRIS ticket bookings created within the last hour"

  case recentFailedBookings of
    [] -> do
      logInfo $ "No failed CRIS ticket bookings found created within the last hour"
      return Complete
    (firstBooking : _) -> do
      integratedBppConfig <- QIntegratedBPPConfig.findById firstBooking.integratedBppConfigId >>= fromMaybeM (InvalidRequest $ "integratedBppConfig not found for id: " <> show firstBooking.integratedBppConfigId)
      case integratedBppConfig.providerConfig of
        DIntegratedBPPConfig.CRIS crisConfig -> do
          (failedOnBothSidesList, failedButSuccessForCrisList) <-
            foldM
              ( \(failedOnBothSides, failedButSuccessForCris) booking -> do
                  result <- withTryCatch "getReconTicketEnquiry:crisRecon" (getReconTicketEnquiry crisConfig booking)
                  case result of
                    Left err -> do
                      logError $ "CRIS recon failed for booking " <> booking.id.getId <> ": " <> show err
                      return (booking.id : failedOnBothSides, failedButSuccessForCris)
                    Right reconResponse -> do
                      let hasSuccessfulTxn = any (\txnDetails -> txnDetails.txnStatus == 1) reconResponse.txnDetailsList
                      if hasSuccessfulTxn
                        then do
                          logInfo $ "CRIS recon succeeded for booking " <> booking.id.getId <> " - found successful transaction"
                          return (failedOnBothSides, booking.id : failedButSuccessForCris)
                        else do
                          logError $ "CRIS recon failed for booking " <> booking.id.getId <> " - no successful transactions found"
                          return (booking.id : failedOnBothSides, failedButSuccessForCris)
              )
              ([], [])
              recentFailedBookings

          logError $ "Failed on both sides (CRIS recon failed): " <> show (map (.getId) failedOnBothSidesList) -- need logs in prod
          logError $ "Failed in our system but success for CRIS: " <> show (map (.getId) failedButSuccessForCrisList) -- need logs in prod
          when scheduleItself $ do
            let newJobData =
                  CrisReconJobData
                    { merchantId = merchantId',
                      merchantOperatingCityId = merchantOperatingCityId',
                      checkSince = fromMaybe 1 crisConfig.reconDuration,
                      scheduleItself
                    }
            createJobIn @_ @'CrisRecon (Just merchantId') (Just merchantOperatingCityId') (intToNominalDiffTime 3600) newJobData

            logInfo $ "Scheduled next CRIS recon job to run in 1 hour"
          return Complete
        _ -> do
          throwError $ InternalError "IntegratedBPPConfig is not a CRIS config"
