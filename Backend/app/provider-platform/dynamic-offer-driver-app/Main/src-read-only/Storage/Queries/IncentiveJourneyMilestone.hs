{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.IncentiveJourneyMilestone where

import qualified Domain.Types.IncentiveJourney
import qualified Domain.Types.IncentiveJourneyMilestone
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.IncentiveJourneyMilestone as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone -> m (Maybe Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByJourneyId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.IncentiveJourney.IncentiveJourney -> m [Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone])
findByJourneyId limit offset journeyId = do findAllWithOptionsKV [Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId)] (Se.Desc Beam.order) limit offset

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone -> m (Maybe Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone -> m ())
updateByPrimaryKey (Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.conditionOperator conditionOperator,
      Se.Set Beam.conditionType conditionType,
      Se.Set Beam.conditionValue conditionValue,
      Se.Set Beam.description description,
      Se.Set Beam.dropSpecialLocationIds dropSpecialLocationIds,
      Se.Set Beam.journeyId (Kernel.Types.Id.getId journeyId),
      Se.Set Beam.order order,
      Se.Set Beam.pickupSpecialLocationIds pickupSpecialLocationIds,
      Se.Set Beam.rewardExpirationAt rewardExpirationAt,
      Se.Set Beam.rewardType rewardType,
      Se.Set Beam.rewardValue rewardValue,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.IncentiveJourneyMilestone Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone where
  fromTType' (Beam.IncentiveJourneyMilestoneT {..}) = do
    pure $
      Just
        Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone
          { conditionOperator = conditionOperator,
            conditionType = conditionType,
            conditionValue = conditionValue,
            createdAt = createdAt,
            description = description,
            dropSpecialLocationIds = dropSpecialLocationIds,
            id = Kernel.Types.Id.Id id,
            journeyId = Kernel.Types.Id.Id journeyId,
            order = order,
            pickupSpecialLocationIds = pickupSpecialLocationIds,
            rewardExpirationAt = rewardExpirationAt,
            rewardType = rewardType,
            rewardValue = rewardValue,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.IncentiveJourneyMilestone Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone where
  toTType' (Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone {..}) = do
    Beam.IncentiveJourneyMilestoneT
      { Beam.conditionOperator = conditionOperator,
        Beam.conditionType = conditionType,
        Beam.conditionValue = conditionValue,
        Beam.createdAt = createdAt,
        Beam.description = description,
        Beam.dropSpecialLocationIds = dropSpecialLocationIds,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.journeyId = Kernel.Types.Id.getId journeyId,
        Beam.order = order,
        Beam.pickupSpecialLocationIds = pickupSpecialLocationIds,
        Beam.rewardExpirationAt = rewardExpirationAt,
        Beam.rewardType = rewardType,
        Beam.rewardValue = rewardValue,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
