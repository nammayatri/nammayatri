{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetRcAssociationDailyStats where

import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Domain.Types.FleetRcAssociationDailyStats
import qualified GHC.Float
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetRcAssociationDailyStats as Beam
import Storage.Queries.Transformers.FleetRcAssociationDailyStats

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetRcAssociationDailyStats.FleetRcAssociationDailyStats -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetRcAssociationDailyStats.FleetRcAssociationDailyStats] -> m ())
createMany = traverse_ create

findByFleetOwnerIdAndRcIdAndMerchantLocalDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Data.Text.Text -> Data.Text.Text -> Data.Time.Calendar.Day -> m (Maybe Domain.Types.FleetRcAssociationDailyStats.FleetRcAssociationDailyStats))
findByFleetOwnerIdAndRcIdAndMerchantLocalDate fleetOwnerId rcId merchantLocalDate = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId,
          Se.Is Beam.rcId $ Se.Eq rcId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateByFleetOwnerIdAndRcIdAndMerchantLocalDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Kernel.Types.Common.Seconds -> Data.Text.Text -> Data.Text.Text -> Data.Time.Calendar.Day -> m ())
updateByFleetOwnerIdAndRcIdAndMerchantLocalDate totalCompletedRides totalEarnings rideDistance rideDuration fleetOwnerId rcId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.totalCompletedRides totalCompletedRides,
      Se.Set Beam.totalEarnings totalEarnings,
      Se.Set Beam.rideDistance (getTotalDistance rideDistance),
      Se.Set Beam.rideDuration rideDuration,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId,
          Se.Is Beam.rcId $ Se.Eq rcId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Data.Text.Text -> Data.Time.Calendar.Day -> Data.Text.Text -> m (Maybe Domain.Types.FleetRcAssociationDailyStats.FleetRcAssociationDailyStats))
findByPrimaryKey fleetOwnerId merchantLocalDate rcId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate,
          Se.Is Beam.rcId $ Se.Eq rcId
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetRcAssociationDailyStats.FleetRcAssociationDailyStats -> m ())
updateByPrimaryKey (Domain.Types.FleetRcAssociationDailyStats.FleetRcAssociationDailyStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency currency,
      Se.Set Beam.distanceUnit distanceUnit,
      Se.Set Beam.rideDistance (getTotalDistance rideDistance),
      Se.Set Beam.rideDuration rideDuration,
      Se.Set Beam.totalCompletedRides totalCompletedRides,
      Se.Set Beam.totalEarnings totalEarnings,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId, Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate, Se.Is Beam.rcId $ Se.Eq rcId]]

instance FromTType' Beam.FleetRcAssociationDailyStats Domain.Types.FleetRcAssociationDailyStats.FleetRcAssociationDailyStats where
  fromTType' (Beam.FleetRcAssociationDailyStatsT {..}) = do
    pure $
      Just
        Domain.Types.FleetRcAssociationDailyStats.FleetRcAssociationDailyStats
          { currency = currency,
            distanceUnit = distanceUnit,
            fleetOwnerId = fleetOwnerId,
            merchantLocalDate = merchantLocalDate,
            rcId = rcId,
            rideDistance = (Kernel.Prelude.fmap (Kernel.Types.Common.Meters . GHC.Float.double2Int) rideDistance),
            rideDuration = rideDuration,
            totalCompletedRides = totalCompletedRides,
            totalEarnings = totalEarnings,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetRcAssociationDailyStats Domain.Types.FleetRcAssociationDailyStats.FleetRcAssociationDailyStats where
  toTType' (Domain.Types.FleetRcAssociationDailyStats.FleetRcAssociationDailyStats {..}) = do
    Beam.FleetRcAssociationDailyStatsT
      { Beam.currency = currency,
        Beam.distanceUnit = distanceUnit,
        Beam.fleetOwnerId = fleetOwnerId,
        Beam.merchantLocalDate = merchantLocalDate,
        Beam.rcId = rcId,
        Beam.rideDistance = getTotalDistance rideDistance,
        Beam.rideDuration = rideDuration,
        Beam.totalCompletedRides = totalCompletedRides,
        Beam.totalEarnings = totalEarnings,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
