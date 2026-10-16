{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Durable end-ride finance for wallet-subscription cities: the same 'processEndRideFinance'
--   the ride-end request forks elsewhere, run as an allocator job that is retried on failure.
--   Every step it runs is idempotent per ride (posting blocks are reference-checked, the fee
--   accrual has a marker, TDS has tdsProcessedAt), so a retry finishes what is missing and
--   never posts twice.
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
import qualified SharedLogic.Finance.WalletEndRidePending as WalletEndRidePending
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
processEndRideFinanceJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
  ride <- QRide.findById jobData.rideId >>= fromMaybeM (RideNotFound jobData.rideId.getId)
  booking <- QRB.findById jobData.bookingId >>= fromMaybeM (BookingNotFound jobData.bookingId.getId)
  if ride.status /= DRide.COMPLETED
    then do
      logWarning $ "ProcessEndRideFinance: ride " <> ride.id.getId <> " is " <> show ride.status <> ", not COMPLETED; dropping its pending marker"
      WalletEndRidePending.clearEndRideFinancePending booking.merchantOperatingCityId ride.id.getId booking.id.getId
      pure Complete
    else do
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
      WalletEndRidePending.clearEndRideFinancePending booking.merchantOperatingCityId ride.id.getId booking.id.getId
      pure Complete
