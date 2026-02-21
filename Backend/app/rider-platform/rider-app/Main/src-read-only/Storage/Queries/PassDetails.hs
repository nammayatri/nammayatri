{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PassDetails where

import qualified Data.Aeson
import qualified Data.Time
import qualified Domain.Types.PassDetails
import qualified Domain.Types.PassOrganization
import qualified Domain.Types.PassType
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PassDetails as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PassDetails.PassDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PassDetails.PassDetails] -> m ())
createMany = traverse_ create

deleteByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.PassType.PassEnum -> m ())
deleteByPersonId personId passEnum = do deleteWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId), Se.Is Beam.passEnum $ Se.Eq passEnum]]

findAllByPassOrganizationId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization -> m [Domain.Types.PassDetails.PassDetails])
findAllByPassOrganizationId limit offset passOrganizationId = do findAllWithOptionsKV [Se.Is Beam.passOrganizationId $ Se.Eq (Kernel.Types.Id.getId passOrganizationId)] (Se.Desc Beam.createdAt) limit offset

findAllByPassOrganizationIdAndVerificationStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization -> Domain.Types.PassDetails.VerificationStatus -> m [Domain.Types.PassDetails.PassDetails])
findAllByPassOrganizationIdAndVerificationStatus limit offset passOrganizationId verificationStatus = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.passOrganizationId $ Se.Eq (Kernel.Types.Id.getId passOrganizationId),
          Se.Is Beam.verificationStatus $ Se.Eq verificationStatus
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PassDetails.PassDetails -> m (Maybe Domain.Types.PassDetails.PassDetails))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.PassType.PassEnum -> m (Maybe Domain.Types.PassDetails.PassDetails))
findByPersonId personId passEnum = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId), Se.Is Beam.passEnum $ Se.Eq passEnum]]

updatePassDetails ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> [Domain.Types.PassDetails.RoutePair] -> Kernel.Prelude.Maybe Data.Time.UTCTime -> Domain.Types.PassDetails.VerificationStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe [Kernel.Prelude.Text] -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.PassType.PassEnum -> m ())
updatePassDetails name passOrganizationId idCardPicture address age guardianName studentClass routePairs graduationDate verificationStatus numberOfStages applicableRouteIds personId passEnum = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.name name,
      Se.Set Beam.passOrganizationId (Kernel.Types.Id.getId passOrganizationId),
      Se.Set Beam.idCardPicture idCardPicture,
      Se.Set Beam.address address,
      Se.Set Beam.age age,
      Se.Set Beam.guardianName guardianName,
      Se.Set Beam.studentClass studentClass,
      Se.Set Beam.routePairs (Just (Kernel.Prelude.toJSON routePairs)),
      Se.Set Beam.graduationDate graduationDate,
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.numberOfStages numberOfStages,
      Se.Set Beam.applicableRouteIds applicableRouteIds,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId),
          Se.Is Beam.passEnum $ Se.Eq passEnum
        ]
    ]

updateVerificationStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.PassDetails.VerificationStatus -> Kernel.Prelude.Maybe Data.Time.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Data.Time.UTCTime -> [Kernel.Types.Id.Id Domain.Types.PassDetails.PassDetails] -> m ())
updateVerificationStatus verificationStatus verificationDate remark graduationDate id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.verificationDate verificationDate,
      Se.Set Beam.remark remark,
      Se.Set Beam.graduationDate graduationDate,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.In (Kernel.Types.Id.getId <$> id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PassDetails.PassDetails -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.PassDetails.PassDetails))
findByPrimaryKey id personId = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PassDetails.PassDetails -> m ())
updateByPrimaryKey (Domain.Types.PassDetails.PassDetails {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.address address,
      Se.Set Beam.age age,
      Se.Set Beam.applicableRouteIds applicableRouteIds,
      Se.Set Beam.graduationDate graduationDate,
      Se.Set Beam.guardianName guardianName,
      Se.Set Beam.idCardPicture idCardPicture,
      Se.Set Beam.name name,
      Se.Set Beam.numberOfStages numberOfStages,
      Se.Set Beam.passEnum passEnum,
      Se.Set Beam.passOrganizationId (Kernel.Types.Id.getId passOrganizationId),
      Se.Set Beam.registerNo registerNo,
      Se.Set Beam.remark remark,
      Se.Set Beam.routePairs (Just (Kernel.Prelude.toJSON routePairs)),
      Se.Set Beam.studentClass studentClass,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.verificationDate verificationDate,
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id),
          Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)
        ]
    ]

instance FromTType' Beam.PassDetails Domain.Types.PassDetails.PassDetails where
  fromTType' (Beam.PassDetailsT {..}) = do
    pure $
      Just
        Domain.Types.PassDetails.PassDetails
          { address = address,
            age = age,
            applicableRouteIds = applicableRouteIds,
            createdAt = createdAt,
            graduationDate = graduationDate,
            guardianName = guardianName,
            id = Kernel.Types.Id.Id id,
            idCardPicture = idCardPicture,
            name = name,
            numberOfStages = numberOfStages,
            passEnum = passEnum,
            passOrganizationId = Kernel.Types.Id.Id passOrganizationId,
            personId = Kernel.Types.Id.Id personId,
            registerNo = registerNo,
            remark = remark,
            routePairs = fromMaybe [] ((\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< routePairs),
            studentClass = studentClass,
            updatedAt = updatedAt,
            verificationDate = verificationDate,
            verificationStatus = verificationStatus,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.PassDetails Domain.Types.PassDetails.PassDetails where
  toTType' (Domain.Types.PassDetails.PassDetails {..}) = do
    Beam.PassDetailsT
      { Beam.address = address,
        Beam.age = age,
        Beam.applicableRouteIds = applicableRouteIds,
        Beam.createdAt = createdAt,
        Beam.graduationDate = graduationDate,
        Beam.guardianName = guardianName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.idCardPicture = idCardPicture,
        Beam.name = name,
        Beam.numberOfStages = numberOfStages,
        Beam.passEnum = passEnum,
        Beam.passOrganizationId = Kernel.Types.Id.getId passOrganizationId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.registerNo = registerNo,
        Beam.remark = remark,
        Beam.routePairs = Just (Kernel.Prelude.toJSON routePairs),
        Beam.studentClass = studentClass,
        Beam.updatedAt = updatedAt,
        Beam.verificationDate = verificationDate,
        Beam.verificationStatus = verificationStatus,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
