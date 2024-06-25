{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.CallPoliceApi where

-- import qualified Data.Aeson as A
-- import qualified Data.Text as T
-- import qualified Domain.Action.UI.Call as DCall
-- import qualified Domain.Types.Booking as DB
-- import Domain.Types.CallStatus
-- import Domain.Types.RideRelatedNotificationConfig
-- import qualified Kernel.Beam.Functions as B
-- import qualified Kernel.External.Call.Interface.Types as Call
-- import Kernel.External.Encryption (decrypt)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
-- import Kernel.Sms.Config (SmsConfig)
-- import Kernel.Types.Error
-- import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler

-- import qualified Storage.CachedQueries.Merchant.MerchantMessage as CMM
-- import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
-- import qualified Storage.Queries.Booking as QB
-- import qualified Storage.Queries.CallStatus as QCallStatus
-- import qualified Storage.Queries.Person as QPerson
-- import qualified Storage.Queries.Ride as QR
-- import qualified Tools.Call as Call
-- import Tools.Notifications
-- import qualified Tools.SMS as Sms

callPoliceApi ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    -- HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig],
    SchedulerFlow r
  ) =>
  Job 'CallPoliceApi ->
  m ()
callPoliceApi Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let _ = jobInfo.jobData
  pure ()
