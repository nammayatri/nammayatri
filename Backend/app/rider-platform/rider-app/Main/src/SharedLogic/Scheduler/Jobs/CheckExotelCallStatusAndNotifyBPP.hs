{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.CheckExotelCallStatusAndNotifyBPP where

import qualified Data.HashMap.Strict as HM
import Domain.Types.CallStatus as DCallStatus hiding (rideId)
import qualified Domain.Types.MerchantOperatingCity as MOC
import qualified Domain.Types.Ride as Ride
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.SessionizerMetrics.Prometheus.Internal
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import SharedLogic.JobScheduler hiding (ScheduledRideNotificationsToRiderJobData (..))
import qualified Storage.CachedQueries.Merchant as SMerchant
import qualified Storage.Queries.CallStatus as QCallStatus
import Tools.Metrics (CoreMetrics)

checkExotelCallStatusAndNotifyBPP ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CoreMetrics m,
    EventStreamFlow m r
  ) =>
  Job 'CheckExotelCallStatusAndNotifyBPP ->
  m ExecutionResult
checkExotelCallStatusAndNotifyBPP Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  let rideId = jobData.rideId
  let bppRideId = jobData.bppRideId
  let merchantId = jobData.merchantId
  let merchantOperatingCityId = jobData.merchantOpCityId
  callStatus <- QCallStatus.findOneByRideId (Just $ getId rideId) >>= fromMaybeM CallStatusDoesNotExist
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  handleCallStatus callStatus bppRideId merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchantOperatingCityId

handleCallStatus ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], MonadFlow m, CoreMetrics m, EventStreamFlow m r) =>
  DCallStatus.CallStatus ->
  Id Ride.BPPRide ->
  Text ->
  BaseUrl ->
  Id MOC.MerchantOperatingCity ->
  m ExecutionResult
handleCallStatus callStatus bppRideId driverOfferApiKey driverOfferBaseUrl merchantOperatingCityId = do
  deploymentVersion <- asks (.version)
  case callStatus.callAttempt of
    Just DCallStatus.Resolved ->
      return Complete
    Just DCallStatus.Failed -> do
      return Complete
    Just DCallStatus.Attempted -> do
      fork "updating in prometheus" $ incrementCounter (getId merchantOperatingCityId) "call_attempt" deploymentVersion.getDeploymentVersion
      -- attempted to call or unregistered Call Stuck
      void $ CallBPPInternal.callCustomerFCM driverOfferApiKey driverOfferBaseUrl (getId bppRideId)
      return Complete
    Just DCallStatus.Pending -> do
      -- Exotel Errors
      void $ CallBPPInternal.callCustomerFCM driverOfferApiKey driverOfferBaseUrl (getId bppRideId)
      return Complete
    Nothing -> throwError (CallStatusFieldNotPresent "callAttempt")
