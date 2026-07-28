{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.IncentiveJourneyStats (module Storage.Queries.IncentiveJourneyStats, module ReExport) where

import qualified Domain.Types.IncentiveJourney
import qualified Domain.Types.IncentiveJourneyMilestone
import qualified Domain.Types.IncentiveJourneyStats
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.IncentiveJourneyStats as Beam
import Storage.Queries.IncentiveJourneyStatsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats] -> m ())
createMany = traverse_ create

findByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats]))
findByDriverId limit offset driverId = do findAllWithOptionsKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)] (Se.Desc Beam.createdAt) limit offset

findByDriverIdAndJourneyId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.IncentiveJourney.IncentiveJourney -> m ([Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats]))
findByDriverIdAndJourneyId limit offset driverId journeyId = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId)
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findByDriverIdAndJourneyIdAndMilestoneIdAndPeriodKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.IncentiveJourney.IncentiveJourney -> Kernel.Types.Id.Id Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone -> Kernel.Prelude.Text -> m (Maybe Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats))
findByDriverIdAndJourneyIdAndMilestoneIdAndPeriodKey driverId journeyId milestoneId periodKey = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId),
          Se.Is Beam.milestoneId $ Se.Eq (Kernel.Types.Id.getId milestoneId),
          Se.Is Beam.periodKey $ Se.Eq periodKey
        ]
    ]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats -> m (Maybe Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats -> m (Maybe Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats -> m ())
updateByPrimaryKey (Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.conditionOperator conditionOperator,
      Se.Set Beam.conditionType conditionType,
      Se.Set Beam.conditionValue conditionValue,
      Se.Set Beam.currentValue currentValue,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.journeyId (Kernel.Types.Id.getId journeyId),
      Se.Set Beam.milestoneId (Kernel.Types.Id.getId milestoneId),
      Se.Set Beam.periodKey periodKey,
      Se.Set Beam.rewardType rewardType,
      Se.Set Beam.rewardValue rewardValue,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
