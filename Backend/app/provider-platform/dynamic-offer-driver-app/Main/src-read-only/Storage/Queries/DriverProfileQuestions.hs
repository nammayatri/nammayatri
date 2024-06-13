{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverProfileQuestions where

import qualified Domain.Types.DriverProfileQuestions
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverProfileQuestions as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverProfileQuestions.DriverProfileQuestions -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverProfileQuestions.DriverProfileQuestions] -> m ())
createMany = traverse_ create

findByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverProfileQuestions.DriverProfileQuestions))
findByPersonId driverId = do findOneWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverProfileQuestions.DriverProfileQuestions))
findByPrimaryKey driverId = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverProfileQuestions.DriverProfileQuestions -> m ())
updateByPrimaryKey (Domain.Types.DriverProfileQuestions.DriverProfileQuestions {..}) = do
  updateWithKV
    [ Se.Set Beam.aspirations aspirations,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.expertAt expertAt,
      Se.Set Beam.hometown hometown,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.pledges pledges,
      Se.Set Beam.whyNY whyNY
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

instance FromTType' Beam.DriverProfileQuestions Domain.Types.DriverProfileQuestions.DriverProfileQuestions where
  fromTType' (Beam.DriverProfileQuestionsT {..}) = do
    pure $
      Just
        Domain.Types.DriverProfileQuestions.DriverProfileQuestions
          { aspirations = aspirations,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            expertAt = expertAt,
            hometown = hometown,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            pledges = pledges,
            whyNY = whyNY
          }

instance ToTType' Beam.DriverProfileQuestions Domain.Types.DriverProfileQuestions.DriverProfileQuestions where
  toTType' (Domain.Types.DriverProfileQuestions.DriverProfileQuestions {..}) = do
    Beam.DriverProfileQuestionsT
      { Beam.aspirations = aspirations,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.expertAt = expertAt,
        Beam.hometown = hometown,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.pledges = pledges,
        Beam.whyNY = whyNY
      }
