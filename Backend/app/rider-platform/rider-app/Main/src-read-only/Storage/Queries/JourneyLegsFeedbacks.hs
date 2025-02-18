{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyLegsFeedbacks where

import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyLegsFeedbacks
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.JourneyLegsFeedbacks as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLegsFeedbacks.JourneyLegsFeedbacks -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.JourneyLegsFeedbacks.JourneyLegsFeedbacks] -> m ())
createMany = traverse_ create

findAllByJourneyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m [Domain.Types.JourneyLegsFeedbacks.JourneyLegsFeedbacks])
findAllByJourneyId journeyId = do findAllWithKV [Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Journey.Journey -> Kernel.Prelude.Int -> m (Maybe Domain.Types.JourneyLegsFeedbacks.JourneyLegsFeedbacks))
findByPrimaryKey journeyId legOrder = do findOneWithKV [Se.And [Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId), Se.Is Beam.legOrder $ Se.Eq legOrder]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLegsFeedbacks.JourneyLegsFeedbacks -> m ())
updateByPrimaryKey (Domain.Types.JourneyLegsFeedbacks.JourneyLegsFeedbacks {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.isExperienceGood isExperienceGood,
      Se.Set Beam.travelMode travelMode,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId), Se.Is Beam.legOrder $ Se.Eq legOrder]]

instance FromTType' Beam.JourneyLegsFeedbacks Domain.Types.JourneyLegsFeedbacks.JourneyLegsFeedbacks where
  fromTType' (Beam.JourneyLegsFeedbacksT {..}) = do
    pure $
      Just
        Domain.Types.JourneyLegsFeedbacks.JourneyLegsFeedbacks
          { isExperienceGood = isExperienceGood,
            journeyId = Kernel.Types.Id.Id journeyId,
            legOrder = legOrder,
            travelMode = travelMode,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.JourneyLegsFeedbacks Domain.Types.JourneyLegsFeedbacks.JourneyLegsFeedbacks where
  toTType' (Domain.Types.JourneyLegsFeedbacks.JourneyLegsFeedbacks {..}) = do
    Beam.JourneyLegsFeedbacksT
      { Beam.isExperienceGood = isExperienceGood,
        Beam.journeyId = Kernel.Types.Id.getId journeyId,
        Beam.legOrder = legOrder,
        Beam.travelMode = travelMode,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
