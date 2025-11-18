{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetRcDailyStats (module Storage.Queries.FleetRcDailyStats, module ReExport) where

import qualified Data.Time.Calendar
import qualified Domain.Types.FleetRcDailyStats
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetRcDailyStats as Beam
import Storage.Queries.FleetRcDailyStatsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetRcDailyStats.FleetRcDailyStats -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetRcDailyStats.FleetRcDailyStats] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Data.Time.Calendar.Day -> Kernel.Prelude.Text -> m (Maybe Domain.Types.FleetRcDailyStats.FleetRcDailyStats))
findByPrimaryKey fleetOwnerId merchantLocalDate rcId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate,
          Se.Is Beam.rcId $ Se.Eq rcId
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetRcDailyStats.FleetRcDailyStats -> m ())
updateByPrimaryKey (Domain.Types.FleetRcDailyStats.FleetRcDailyStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency currency,
      Se.Set Beam.rideDistance rideDistance,
      Se.Set Beam.rideDuration rideDuration,
      Se.Set Beam.totalCompletedRides totalCompletedRides,
      Se.Set Beam.totalEarnings totalEarnings,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId, Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate, Se.Is Beam.rcId $ Se.Eq rcId]]
