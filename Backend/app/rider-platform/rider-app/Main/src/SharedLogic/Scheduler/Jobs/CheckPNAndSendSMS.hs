{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.CheckPNAndSendSMS where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler (RiderJobType (..))
import qualified SharedLogic.MessageBuilder as MessageBuilder
import Storage.CachedQueries.FollowRide
import qualified Tools.SMS as Sms

checkPNAndSendSMS ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EncFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig]
  ) =>
  Job 'CheckPNAndSendSMS ->
  m ExecutionResult
checkPNAndSendSMS Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  personFollowsRide <- isKeyExists jobData.redisKey
  if personFollowsRide
    then return Complete
    else do
      sendSMS jobData.merchantId jobData.merchantOpCityId jobData.emergencyContactNumber jobData.riderName jobData.trackLink
      return Complete

sendSMS ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EncFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig]
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Maybe Text ->
  Text ->
  m ()
sendSMS merchantId merchantOpCity phoneNo name trackLink = do
  smsCfg <- asks (.smsCfg)
  let sender = smsCfg.sender
  message <-
    MessageBuilder.buildFollowRideStartedMessage merchantOpCity $
      MessageBuilder.BuildFollowRideMessageReq
        { userName = fromMaybe "" name,
          rideLink = trackLink
        }
  void $
    Sms.sendSMS merchantId merchantOpCity (Sms.SendSMSReq message phoneNo sender)
      >>= Sms.checkSmsResult
