{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.UpdateCrisUtsData where

import Domain.Types.IntegratedBPPConfig
import ExternalBPP.ExternalAPI.Subway.CRIS.Uts as Uts
import Kernel.External.Encryption
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.IntegratedBPPConfig as QIntegratedBPPConfig

updateCrisUtsDataJob ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r
  ) =>
  Job 'UpdateCrisUtsData ->
  m ExecutionResult
updateCrisUtsDataJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      integratedBppConfigId = jobData.integratedBPPConfigId
  integratedBppConfig <- QIntegratedBPPConfig.findById integratedBppConfigId >>= fromMaybeM (InvalidRequest $ "integratedBppConfig not found for id: " <> show integratedBppConfigId)
  case integratedBppConfig.providerConfig of
    CRIS config@CRISConfig {..} -> do
      utsData <- Uts.getUtsData config
      encryptedAgentDataKey <- encrypt utsData.tkt
      encryptedEncryptionKey <- encrypt utsData.url
      encryptedDecryptionKey <- encrypt utsData.fare
      let updatedProviderConfig =
            CRIS $
              CRISConfig
                { encryptionKey = encryptedEncryptionKey,
                  decryptionKey = encryptedDecryptionKey,
                  agentDataDecryptionKey = encryptedAgentDataKey,
                  ..
                }
      QIntegratedBPPConfig.updateByPrimaryKey integratedBppConfig {providerConfig = updatedProviderConfig}
      let scheduleAfter = secondsToNominalDiffTime (24 * 60 * 60)
          newJobData = UpdateCrisUtsDataJobData {integratedBPPConfigId = integratedBppConfigId}
      createJobIn @_ @'UpdateCrisUtsData (Just integratedBppConfig.merchantId) (Just integratedBppConfig.merchantOperatingCityId) scheduleAfter (newJobData :: UpdateCrisUtsDataJobData)
      return Complete
    _ -> throwError $ InternalError $ "Incorrect bpp config with id " <> (show $ integratedBppConfig.id)
