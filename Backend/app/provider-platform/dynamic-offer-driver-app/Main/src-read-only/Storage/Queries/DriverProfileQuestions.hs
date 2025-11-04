{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverProfileQuestions (module Storage.Queries.DriverProfileQuestions, module ReExport) where

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
import Storage.Queries.DriverProfileQuestionsExtra as ReExport

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
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.aboutMe aboutMe,
      Se.Set Beam.aspirations aspirations,
      Se.Set Beam.drivingSince drivingSince,
      Se.Set Beam.hometown hometown,
      Se.Set Beam.imageIds imageIds,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.pledges pledges,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleTags vehicleTags
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]
