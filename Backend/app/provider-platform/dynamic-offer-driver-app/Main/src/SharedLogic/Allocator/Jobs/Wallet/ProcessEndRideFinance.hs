{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Durable end-ride finance, in every city: 'processEndRideFinance' run as an allocator job that is
--   retried on failure (it used to be a fork on the ride-end request).
--   Every step it runs is idempotent per ride (each posting is checked against one read of the ride's
--   ledger entries, invoices are created only if missing, the fee accrual has a marker, TDS has
--   tdsProcessedAt), so a retry finishes what is missing and never posts twice.
module SharedLogic.Allocator.Jobs.Wallet.ProcessEndRideFinance
  ( processEndRideFinanceJob,
  )
where

import qualified Data.Map as M
import Domain.Action.UI.Ride.EndRide.Internal (processEndRideFinance)
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import SharedLogic.Allocator
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FareParameters as QFare
import qualified Storage.Queries.Ride as QRide
import Tools.Error

processEndRideFinanceJob ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    Finance.HasActorInfo m r,
    Esq.EsqDBReplicaFlow m r,
    HasField "maxShards" r Int,
    EventStreamFlow m r,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    LT.HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "blackListedJobs" r [Text],
    HasField "activeDriversListKeyShards" r Int,
    HasField "enableDriverFeeShardedFanOut" r Bool,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    BeamFlow m r
  ) =>
  Job 'ProcessEndRideFinance ->
  m ExecutionResult
processEndRideFinanceJob Job {id, jobInfo, createdAt} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
  ride <- QRide.findById jobData.rideId >>= fromMaybeM (RideNotFound jobData.rideId.getId)
  booking <- QRB.findById jobData.bookingId >>= fromMaybeM (BookingNotFound jobData.bookingId.getId)
  now <- getCurrentTime
  case ride.status of
    DRide.CANCELLED -> do
      logWarning $ "ProcessEndRideFinance: ride " <> ride.id.getId <> " is CANCELLED; nothing to post"
      pure Complete
    DRide.COMPLETED -> do
      merchant <- CQM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
      -- The fare params the ride ended with: persisted by the end-ride transaction and linked on the
      -- ride; the booking's own when the fare wasn't recomputed.
      fareParams <- case ride.fareParametersId of
        Just fareParamsId -> fromMaybe booking.fareParams <$> QFare.findById fareParamsId
        Nothing -> pure booking.fareParams
      driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM DriverInfoNotFound
      transporterConfig <-
        getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing
          >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
      processEndRideFinance merchant ride booking fareParams (cast ride.driverId) driverInfo transporterConfig
      pure Complete
    -- The job is created as the ride is completed, so a ride not seen as COMPLETED yet is a read that is
    -- behind: try again shortly, for at most an hour, rather than drop the ride's postings.
    _
      | diffUTCTime now createdAt < endRideStatusWaitCap -> do
        logWarning $ "ProcessEndRideFinance: ride " <> ride.id.getId <> " is " <> show ride.status <> ", not COMPLETED yet; retrying in a minute"
        pure $ ReSchedule (addUTCTime 60 now)
      | otherwise -> do
        logError $ "ProcessEndRideFinance: ride " <> ride.id.getId <> " still " <> show ride.status <> " an hour after its end-ride job was created; giving up, needs a manual check"
        pure Complete

endRideStatusWaitCap :: NominalDiffTime
endRideStatusWaitCap = 3600
