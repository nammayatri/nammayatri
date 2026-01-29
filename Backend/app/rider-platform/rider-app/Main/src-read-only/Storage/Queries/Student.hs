{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Student (module Storage.Queries.Student, module ReExport) where

import qualified Domain.Types.College
import qualified Domain.Types.Person
import qualified Domain.Types.Student
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Student as Beam
import Storage.Queries.StudentExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Student.Student -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Student.Student] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Student.Student -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Student.Student -> m (Maybe Domain.Types.Student.Student))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.Student.Student))
findByPersonId personId = do findOneWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateStops ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> [Kernel.Prelude.Text] -> Kernel.Types.Id.Id Domain.Types.Student.Student -> m ())
updateStops sourceStop destinationStop intermediateStops id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.sourceStop sourceStop,
      Se.Set Beam.destinationStop destinationStop,
      Se.Set Beam.intermediateStops (Just intermediateStops),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStudentDetails ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.College.College -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Student.Student -> m ())
updateStudentDetails studentName collegeId collegeName studentPicture studentAddress studentAge guardianName studentClass graduationDate id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.studentName studentName,
      Se.Set Beam.collegeId (Kernel.Types.Id.getId collegeId),
      Se.Set Beam.collegeName collegeName,
      Se.Set Beam.studentPicture studentPicture,
      Se.Set Beam.studentAddress studentAddress,
      Se.Set Beam.studentAge studentAge,
      Se.Set Beam.guardianName guardianName,
      Se.Set Beam.studentClass studentClass,
      Se.Set Beam.graduationDate graduationDate,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateVerificationStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.Student.VerificationStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Student.Student -> m ())
updateVerificationStatus verificationStatus verificationDate id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.verificationDate verificationDate, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Student.Student -> m (Maybe Domain.Types.Student.Student))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Student.Student -> m ())
updateByPrimaryKey (Domain.Types.Student.Student {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.collegeId (Kernel.Types.Id.getId collegeId),
      Se.Set Beam.collegeName collegeName,
      Se.Set Beam.destinationStop destinationStop,
      Se.Set Beam.graduationDate graduationDate,
      Se.Set Beam.guardianName guardianName,
      Se.Set Beam.intermediateStops (Just intermediateStops),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.sourceStop sourceStop,
      Se.Set Beam.studentAddress studentAddress,
      Se.Set Beam.studentAge studentAge,
      Se.Set Beam.studentClass studentClass,
      Se.Set Beam.studentName studentName,
      Se.Set Beam.studentPicture studentPicture,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.verificationDate verificationDate,
      Se.Set Beam.verificationStatus verificationStatus
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
