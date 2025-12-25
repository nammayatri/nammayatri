{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyFeedback where

import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyFeedback
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.JourneyFeedback as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyFeedback.JourneyFeedback -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.JourneyFeedback.JourneyFeedback] -> m ())
createMany = traverse_ create

findAllByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.JourneyFeedback.JourneyFeedback])
findAllByRiderId riderId = do findAllWithKV [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]

findByJourneyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m (Maybe Domain.Types.JourneyFeedback.JourneyFeedback))
findByJourneyId journeyId = do findOneWithKV [Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m (Maybe Domain.Types.JourneyFeedback.JourneyFeedback))
findByPrimaryKey journeyId = do findOneWithKV [Se.And [Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyFeedback.JourneyFeedback -> m ())
updateByPrimaryKey (Domain.Types.JourneyFeedback.JourneyFeedback {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.additionalFeedBack additionalFeedBack,
      Se.Set Beam.rating rating,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId)]]

instance FromTType' Beam.JourneyFeedback Domain.Types.JourneyFeedback.JourneyFeedback where
  fromTType' (Beam.JourneyFeedbackT {..}) = do
    pure $
      Just
        Domain.Types.JourneyFeedback.JourneyFeedback
          { additionalFeedBack = additionalFeedBack,
            journeyId = Kernel.Types.Id.Id journeyId,
            rating = rating,
            riderId = Kernel.Types.Id.Id riderId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.JourneyFeedback Domain.Types.JourneyFeedback.JourneyFeedback where
  toTType' (Domain.Types.JourneyFeedback.JourneyFeedback {..}) = do
    Beam.JourneyFeedbackT
      { Beam.additionalFeedBack = additionalFeedBack,
        Beam.journeyId = Kernel.Types.Id.getId journeyId,
        Beam.rating = rating,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
