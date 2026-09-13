{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStats (module Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStats, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats
import qualified Lib.IncentiveJourney.Storage.Beam.BeamFlow
import qualified Lib.IncentiveJourney.Storage.Beam.IncentiveJourneyStats as Beam
import Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats -> m ())
create = createWithKV

createMany :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats] -> m ())
createMany = traverse_ create

findById ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats -> m (Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPersonId ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.Person -> m ([Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats]))
findByPersonId limit offset personId = do findAllWithOptionsKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)] (Se.Desc Beam.createdAt) limit offset

findByPersonIdAndJourneyId ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.Person -> Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney -> m ([Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats]))
findByPersonIdAndJourneyId limit offset personId journeyId = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId),
          Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId)
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findByPersonIdAndJourneyIdAndMilestoneIdAndPeriodKey ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.Person -> Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney -> Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone -> Kernel.Prelude.Text -> m (Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats))
findByPersonIdAndJourneyIdAndMilestoneIdAndPeriodKey personId journeyId milestoneId periodKey = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId),
          Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId),
          Se.Is Beam.milestoneId $ Se.Eq (Kernel.Types.Id.getId milestoneId),
          Se.Is Beam.periodKey $ Se.Eq periodKey
        ]
    ]

findByPrimaryKey ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats -> m (Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats -> m ())
updateByPrimaryKey (Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.conditionOperator conditionOperator,
      Se.Set Beam.conditionType conditionType,
      Se.Set Beam.conditionValue conditionValue,
      Se.Set Beam.currentValue currentValue,
      Se.Set Beam.journeyId (Kernel.Types.Id.getId journeyId),
      Se.Set Beam.milestoneId (Kernel.Types.Id.getId milestoneId),
      Se.Set Beam.periodKey periodKey,
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.rewardType rewardType,
      Se.Set Beam.rewardValue rewardValue,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
