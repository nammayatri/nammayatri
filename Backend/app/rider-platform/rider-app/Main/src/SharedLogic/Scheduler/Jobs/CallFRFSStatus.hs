{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.CallFRFSStatus where

import Data.Time hiding (getCurrentTime)
import Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import Environment
import qualified ExternalBPP.CallAPI as CallExternalBPP
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.FRFSTicketBooking as QB
import qualified Storage.Queries.MerchantOperatingCity as DMOC
import Tools.Error

callFRFSStatus ::
  Job 'CallFRFSStatus ->
  Flow ExecutionResult
callFRFSStatus Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      bookingId = jobData.bookingId
      bapConfig = jobData.bapConfig
      platformType = jobData.platformType
  booking <- B.runInReplica $ QB.findById bookingId >>= fromMaybeM (FRFSTicketBookingDoesNotExist bookingId.getId)
  let merchantOperatingCityId = booking.merchantOperatingCityId
      merchantId = booking.merchantId
      key = mkRescheduleKey bookingId
  merchantOperatingCity <- B.runInReplica $ DMOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOperatingCityId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
  now <- getCurrentTime
  let dayEnd = getMidnightOfNextDay now
  prevDelay <- Hedis.withCrossAppRedis $ Hedis.get key
  when (now >= dayEnd) $ do
    -- update in Db (cancellation reason - bpp didn't respond)
    Hedis.withCrossAppRedis $ Hedis.del key
  case booking.status of
    CONFIRMED -> skipJob bookingId
    CANCELLED -> skipJob bookingId
    _ -> do
      void $ CallExternalBPP.status merchantId merchantOperatingCity bapConfig booking platformType
      ReSchedule <$> getRescheduledTime (fromMaybe riderConfig.policeTriggerDelay prevDelay) bookingId
  where
    skipJob bookingId = do
      logDebug $ "TimeValidation for this Job has been completed." <> bookingId.getId <> "skipping calling FRFS Status Job"
      return Complete

getRescheduledTime :: NominalDiffTime -> Id DFRFSTicketBooking.FRFSTicketBooking -> Flow UTCTime
getRescheduledTime prevDelay bookingId = do
  now <- getCurrentTime
  let newDelay = prevDelay * 3
      dayEnd = getMidnightOfNextDay now
      newTime = addUTCTime newDelay now

  Hedis.withCrossAppRedis $ Hedis.set (mkRescheduleKey bookingId) newDelay

  return $ min newTime dayEnd

mkRescheduleKey :: Id DFRFSTicketBooking.FRFSTicketBooking -> Text
mkRescheduleKey bookingId = "FRFSStatusJobUpadtedDelay:" <> bookingId.getId

getMidnightOfNextDay :: UTCTime -> UTCTime
getMidnightOfNextDay now =
  let (year, month, day) = toGregorian (utctDay now)
      nextDay = addDays 1 (fromGregorian year month day)
   in UTCTime nextDay 0
