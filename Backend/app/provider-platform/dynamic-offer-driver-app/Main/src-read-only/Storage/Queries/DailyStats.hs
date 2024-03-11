{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DailyStats where

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
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, KvDbFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DailyStats as Beam

create :: KvDbFlow m r => Domain.Types.DailyStats.DailyStats -> m ()
create = createWithKV

createMany :: KvDbFlow m r => [Domain.Types.DailyStats.DailyStats] -> m ()
createMany = traverse_ create

findByDriverIdAndDate :: KvDbFlow m r => Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> m (Maybe (Domain.Types.DailyStats.DailyStats))
findByDriverIdAndDate (Kernel.Types.Id.Id driverId) merchantLocalDate = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq driverId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateByDriverId :: KvDbFlow m r => Kernel.Types.Common.Money -> Kernel.Prelude.Int -> Kernel.Types.Common.Meters -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> m ()
updateByDriverId totalEarnings numRides totalDistance (Kernel.Types.Id.Id driverId) merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.totalEarnings totalEarnings,
      Se.Set Beam.numRides numRides,
      Se.Set Beam.totalDistance totalDistance,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq driverId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

findByPrimaryKey :: KvDbFlow m r => Data.Text.Text -> m (Maybe (Domain.Types.DailyStats.DailyStats))
findByPrimaryKey id = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: KvDbFlow m r => Domain.Types.DailyStats.DailyStats -> m ()
updateByPrimaryKey Domain.Types.DailyStats.DailyStats {..} = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.merchantLocalDate merchantLocalDate,
      Se.Set Beam.numRides numRides,
      Se.Set Beam.totalDistance totalDistance,
      Se.Set Beam.totalEarnings totalEarnings,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

instance FromTType' Beam.DailyStats Domain.Types.DailyStats.DailyStats where
  fromTType' Beam.DailyStatsT {..} = do
    pure $
      Just
        Domain.Types.DailyStats.DailyStats
          { driverId = Kernel.Types.Id.Id driverId,
            id = id,
            merchantLocalDate = merchantLocalDate,
            numRides = numRides,
            totalDistance = totalDistance,
            totalEarnings = totalEarnings,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DailyStats Domain.Types.DailyStats.DailyStats where
  toTType' Domain.Types.DailyStats.DailyStats {..} = do
    Beam.DailyStatsT
      { Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = id,
        Beam.merchantLocalDate = merchantLocalDate,
        Beam.numRides = numRides,
        Beam.totalDistance = totalDistance,
        Beam.totalEarnings = totalEarnings,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
