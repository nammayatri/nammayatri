{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverGracePeriodTracker (module Storage.Queries.DriverGracePeriodTracker, module ReExport) where

import qualified Domain.Types.DriverGracePeriodTracker
import qualified Domain.Types.PenaltyRule
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverGracePeriodTracker as Beam
import Storage.Queries.DriverGracePeriodTrackerExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker -> m (Maybe Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByDriverIdAndRuleId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PenaltyRule.PenaltyRule -> m (Maybe Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker))
findByDriverIdAndRuleId driverId ruleId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.ruleId $ Se.Eq (Kernel.Types.Id.getId ruleId)
        ]
    ]

findAllByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker])
findAllByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateOffenseCountById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker -> m ())
updateOffenseCountById offenseCount updatedAt id = do
  updateWithKV
    [ Se.Set Beam.offenseCount offenseCount,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateWindowById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker -> m ())
updateWindowById offenseCount windowStartTime windowEndTime updatedAt id = do
  updateWithKV
    [ Se.Set Beam.offenseCount offenseCount,
      Se.Set Beam.windowStartTime windowStartTime,
      Se.Set Beam.windowEndTime windowEndTime,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker -> m (Maybe Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker -> m ())
updateByPrimaryKey (Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker {..}) = do
  updateWithKV
    [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.ruleId (Kernel.Types.Id.getId ruleId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.offenseCount offenseCount,
      Se.Set Beam.windowStartTime windowStartTime,
      Se.Set Beam.windowEndTime windowEndTime,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DriverGracePeriodTracker Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker where
  fromTType' (Beam.DriverGracePeriodTrackerT {..}) = do
    pure $
      Just
        Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker
          { id = Kernel.Types.Id.Id id,
            driverId = Kernel.Types.Id.Id driverId,
            ruleId = Kernel.Types.Id.Id ruleId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            offenseCount = offenseCount,
            windowStartTime = windowStartTime,
            windowEndTime = windowEndTime,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverGracePeriodTracker Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker where
  toTType' (Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker {..}) = do
    Beam.DriverGracePeriodTrackerT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.ruleId = Kernel.Types.Id.getId ruleId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.offenseCount = offenseCount,
        Beam.windowStartTime = windowStartTime,
        Beam.windowEndTime = windowEndTime,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
