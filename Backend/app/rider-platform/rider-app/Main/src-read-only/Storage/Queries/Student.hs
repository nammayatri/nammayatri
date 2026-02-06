{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Student where

import qualified Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Organization
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

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Student.Student -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Student.Student] -> m ())
createMany = traverse_ create

deleteByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByPersonId personId = do deleteWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findAllByOrganizationId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Organization.Organization -> m ([Domain.Types.Student.Student]))
findAllByOrganizationId limit offset organizationId = do findAllWithOptionsKV [Se.Is Beam.organizationId $ Se.Eq (Kernel.Types.Id.getId organizationId)] (Se.Desc Beam.createdAt) limit offset

findAllByOrganizationIdAndVerificationStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Organization.Organization -> Domain.Types.Student.VerificationStatus -> m ([Domain.Types.Student.Student]))
findAllByOrganizationIdAndVerificationStatus limit offset organizationId verificationStatus = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.organizationId $ Se.Eq (Kernel.Types.Id.getId organizationId),
          Se.Is Beam.verificationStatus $ Se.Eq verificationStatus
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Student.Student -> m (Maybe Domain.Types.Student.Student))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.Student.Student))
findByPersonId personId = do findOneWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateStudentDetails ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Organization.Organization -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> [Domain.Types.Student.RoutePair] -> Kernel.Prelude.Maybe Data.Time.UTCTime -> Domain.Types.Student.VerificationStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateStudentDetails name organizationId idCardPicture address age guardianName studentClass routePairs graduationDate verificationStatus numberOfStages personId = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.name name,
      Se.Set Beam.organizationId (Kernel.Types.Id.getId organizationId),
      Se.Set Beam.idCardPicture idCardPicture,
      Se.Set Beam.address address,
      Se.Set Beam.age age,
      Se.Set Beam.guardianName guardianName,
      Se.Set Beam.studentClass studentClass,
      Se.Set Beam.routePairs (Just (Kernel.Prelude.toJSON routePairs)),
      Se.Set Beam.graduationDate graduationDate,
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.numberOfStages numberOfStages,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateVerificationStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.Student.VerificationStatus -> Kernel.Prelude.Maybe Data.Time.UTCTime -> Kernel.Prelude.Maybe Data.Time.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateVerificationStatus verificationStatus verificationDate graduationDate remark personId = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.verificationDate verificationDate,
      Se.Set Beam.graduationDate graduationDate,
      Se.Set Beam.remark remark,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Student.Student -> m (Maybe Domain.Types.Student.Student))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Student.Student -> m ())
updateByPrimaryKey (Domain.Types.Student.Student {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.address address,
      Se.Set Beam.age age,
      Se.Set Beam.graduationDate graduationDate,
      Se.Set Beam.guardianName guardianName,
      Se.Set Beam.idCardPicture idCardPicture,
      Se.Set Beam.name name,
      Se.Set Beam.numberOfStages numberOfStages,
      Se.Set Beam.organizationId (Kernel.Types.Id.getId organizationId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.remark remark,
      Se.Set Beam.routePairs (Just (Kernel.Prelude.toJSON routePairs)),
      Se.Set Beam.studentClass studentClass,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.verificationDate verificationDate,
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Student Domain.Types.Student.Student where
  fromTType' (Beam.StudentT {..}) = do
    pure $
      Just
        Domain.Types.Student.Student
          { address = address,
            age = age,
            createdAt = createdAt,
            graduationDate = graduationDate,
            guardianName = guardianName,
            id = Kernel.Types.Id.Id id,
            idCardPicture = idCardPicture,
            name = name,
            numberOfStages = numberOfStages,
            organizationId = Kernel.Types.Id.Id organizationId,
            personId = Kernel.Types.Id.Id personId,
            remark = remark,
            routePairs = fromMaybe [] ((\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< routePairs),
            studentClass = studentClass,
            updatedAt = updatedAt,
            verificationDate = verificationDate,
            verificationStatus = verificationStatus,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.Student Domain.Types.Student.Student where
  toTType' (Domain.Types.Student.Student {..}) = do
    Beam.StudentT
      { Beam.address = address,
        Beam.age = age,
        Beam.createdAt = createdAt,
        Beam.graduationDate = graduationDate,
        Beam.guardianName = guardianName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.idCardPicture = idCardPicture,
        Beam.name = name,
        Beam.numberOfStages = numberOfStages,
        Beam.organizationId = Kernel.Types.Id.getId organizationId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.remark = remark,
        Beam.routePairs = Just (Kernel.Prelude.toJSON routePairs),
        Beam.studentClass = studentClass,
        Beam.updatedAt = updatedAt,
        Beam.verificationDate = verificationDate,
        Beam.verificationStatus = verificationStatus,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
