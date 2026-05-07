{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PassDetails (module Storage.Queries.PassDetails, module ReExport) where

import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
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
import Storage.Queries.PassDetailsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PassDetails.PassDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PassDetails.PassDetails] -> m ())
createMany = traverse_ create

findAllByMerchantIdAndMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.PassType.PassEnum -> [Domain.Types.PassDetails.VerificationStatus] -> m [Domain.Types.PassDetails.PassDetails])
findAllByMerchantIdAndMerchantOperatingCityId limit offset merchantId merchantOperatingCityId passEnum verificationStatus = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.passEnum $ Se.Eq passEnum,
          Se.Is Beam.verificationStatus $ Se.In verificationStatus
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findAllByPassOrganizationIdAndVerificationStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization -> [Domain.Types.PassDetails.VerificationStatus] -> m [Domain.Types.PassDetails.PassDetails])
findAllByPassOrganizationIdAndVerificationStatus limit offset passOrganizationId verificationStatus = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.passOrganizationId $ Se.Eq (Kernel.Types.Id.getId passOrganizationId),
          Se.Is Beam.verificationStatus $ Se.In verificationStatus
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
  (Domain.Types.PassDetails.VerificationStatus -> Data.Time.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Data.Time.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> [Kernel.Types.Id.Id Domain.Types.PassDetails.PassDetails] -> m ())
updateVerificationStatus verificationStatus validTill remark graduationDate numberOfStages id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.remark remark,
      Se.Set Beam.graduationDate graduationDate,
      Se.Set Beam.numberOfStages numberOfStages,
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
    [ Se.Set Beam.aadharNoEncrypted (aadharNo <&> unEncrypted . (.encrypted)),
      Se.Set Beam.aadharNoHash (aadharNo <&> (.hash)),
      Se.Set Beam.address address,
      Se.Set Beam.age age,
      Se.Set Beam.applicableRouteIds applicableRouteIds,
      Se.Set Beam.gender gender,
      Se.Set Beam.graduationDate graduationDate,
      Se.Set Beam.guardianName guardianName,
      Se.Set Beam.idCardPicture idCardPicture,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.numberOfStages numberOfStages,
      Se.Set Beam.passEnum passEnum,
      Se.Set Beam.passOrganizationId (Kernel.Types.Id.getId passOrganizationId),
      Se.Set Beam.pincode pincode,
      Se.Set Beam.referenceNumber referenceNumber,
      Se.Set Beam.registerNo registerNo,
      Se.Set Beam.remark remark,
      Se.Set Beam.routePairs (Just (Kernel.Prelude.toJSON routePairs)),
      Se.Set Beam.selfImage selfImage,
      Se.Set Beam.studentClass studentClass,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.verificationStatus verificationStatus
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]
