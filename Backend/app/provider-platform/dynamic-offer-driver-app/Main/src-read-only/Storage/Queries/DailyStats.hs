{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DailyStats (module Storage.Queries.DailyStats, module ReExport) where

import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Domain.Types.DailyStats
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DailyStats as Beam
import Storage.Queries.DailyStatsExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.DailyStats.DailyStats -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.DailyStats.DailyStats] -> m ())
createMany = traverse_ create

findByDriverIdAndDate :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> m (Maybe Domain.Types.DailyStats.DailyStats))
findByDriverIdAndDate (Kernel.Types.Id.Id driverId) merchantLocalDate = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq driverId, Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate]]

updateByDriverId ::
  KvDbFlow m r =>
  (Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Int -> Kernel.Types.Common.Meters -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> m ())
updateByDriverId totalEarnings numRides totalDistance (Kernel.Types.Id.Id driverId) merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.totalEarnings (Kernel.Prelude.roundToIntegral totalEarnings),
      Se.Set Beam.totalEarningsAmount (Kernel.Prelude.Just totalEarnings),
      Se.Set Beam.numRides numRides,
      Se.Set Beam.totalDistance totalDistance,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq driverId, Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate]]

findByPrimaryKey :: KvDbFlow m r => (Data.Text.Text -> m (Maybe Domain.Types.DailyStats.DailyStats))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.DailyStats.DailyStats -> m ())
updateByPrimaryKey (Domain.Types.DailyStats.DailyStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.merchantLocalDate merchantLocalDate,
      Se.Set Beam.numRides numRides,
      Se.Set Beam.totalDistance totalDistance,
      Se.Set Beam.totalEarnings (Kernel.Prelude.roundToIntegral totalEarnings),
      Se.Set Beam.totalEarningsAmount (Kernel.Prelude.Just totalEarnings),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq id]]
