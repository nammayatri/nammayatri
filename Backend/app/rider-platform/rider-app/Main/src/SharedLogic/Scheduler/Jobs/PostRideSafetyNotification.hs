{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.PostRideSafetyNotification where

import qualified Kernel.Beam.Functions as B
import Kernel.External.Notification
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QR
import Tools.Error
import Tools.Notifications

postRideSafetyNotification ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasKafkaProducer r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'PostRideSafetyNotification ->
  m ExecutionResult
postRideSafetyNotification Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      personId = jobData.personId
      rideId = jobData.rideId
  ride <- B.runInReplica $ QR.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  when (isNothing ride.wasRideSafe && isNothing ride.rideRating) $ do
    riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
    let entityData = NotifReq {title = "Did you have a safe journey?", message = "Thank you for riding with us. Please share your ride experience."}
    logDebug "Triggering notification for post ride safety check"
    notifyPersonOnEvents person entityData POST_RIDE_SAFETY_CHECK
    let scheduleAfter = riderConfig.ivrTriggerDelay
        safetyIvrJobData = SafetyIVRJobData {rideId = ride.id, personId = personId}
    createJobIn @_ @'SafetyIVR ride.merchantId ride.merchantOperatingCityId scheduleAfter (safetyIvrJobData :: SafetyIVRJobData)
    logDebug "Created Safety IVR Job"
  return Complete
