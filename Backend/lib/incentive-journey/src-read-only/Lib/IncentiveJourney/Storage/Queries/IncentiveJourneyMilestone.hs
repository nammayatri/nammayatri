{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyMilestone (module Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyMilestone, module ReExport) where

import qualified Data.Text
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone
import qualified Lib.IncentiveJourney.Storage.Beam.BeamFlow
import qualified Lib.IncentiveJourney.Storage.Beam.IncentiveJourneyMilestone as Beam
import Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyMilestoneExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone -> m ())
create = createWithKV

createMany :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone] -> m ())
createMany = traverse_ create

findById ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone -> m (Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByJourneyId ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney -> m [Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone])
findByJourneyId limit offset journeyId = do findAllWithOptionsKV [Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId)] (Se.Desc Beam.order) limit offset

findByPrimaryKey ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone -> m (Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone -> m ())
updateByPrimaryKey (Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.areaType (Kernel.Prelude.fmap (Data.Text.pack . Kernel.Prelude.show) areaType),
      Se.Set Beam.conditionOperator conditionOperator,
      Se.Set Beam.conditionType conditionType,
      Se.Set Beam.conditionValue conditionValue,
      Se.Set Beam.description description,
      Se.Set Beam.journeyId (Kernel.Types.Id.getId journeyId),
      Se.Set Beam.name name,
      Se.Set Beam.order order,
      Se.Set Beam.rewardExpirationAt rewardExpirationAt,
      Se.Set Beam.rewardMetadata rewardMetadata,
      Se.Set Beam.rewardType rewardType,
      Se.Set Beam.rewardValue rewardValue,
      Se.Set Beam.serviceTierType (Kernel.Prelude.fmap (Data.Text.pack . Kernel.Prelude.show) serviceTierType),
      Se.Set Beam.specialLocationIds specialLocationIds,
      Se.Set Beam.timeBounds timeBounds,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleCategory (Kernel.Prelude.fmap (Data.Text.pack . Kernel.Prelude.show) vehicleCategory),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
