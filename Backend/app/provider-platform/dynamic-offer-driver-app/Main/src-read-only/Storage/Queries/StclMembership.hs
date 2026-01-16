{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.StclMembership (module Storage.Queries.StclMembership, module ReExport) where

import qualified Domain.Types.Person
import qualified Domain.Types.StclMembership
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.StclMembership as Beam
import Storage.Queries.StclMembershipExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.StclMembership.StclMembership -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.StclMembership.StclMembership] -> m ())
createMany = traverse_ create

findByApplicationId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.StclMembership.StclMembership))
findByApplicationId applicationId = do findOneWithKV [Se.Is Beam.applicationId $ Se.Eq applicationId]

findByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.StclMembership.StclMembership]))
findByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByDriverIdAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.StclMembership.ApplicationStatus -> m ([Domain.Types.StclMembership.StclMembership]))
findByDriverIdAndStatus driverId status = do findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.status $ Se.Eq status]]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.StclMembership.StclMembership -> m (Maybe Domain.Types.StclMembership.StclMembership))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.StclMembership.ApplicationStatus -> Kernel.Types.Id.Id Domain.Types.StclMembership.StclMembership -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.StclMembership.StclMembership -> m (Maybe Domain.Types.StclMembership.StclMembership))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.StclMembership.StclMembership -> m ())
updateByPrimaryKey (Domain.Types.StclMembership.StclMembership {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.aadharNumberEncrypted (((unEncrypted . (.encrypted) $ aadharNumber))),
      Se.Set Beam.aadharNumberHash (((.hash) aadharNumber)),
      Se.Set Beam.accountNumberEncrypted (((unEncrypted . (.encrypted) $ accountNumber))),
      Se.Set Beam.accountNumberHash (((.hash) accountNumber)),
      Se.Set Beam.addressCity addressCity,
      Se.Set Beam.addressPostalCode addressPostalCode,
      Se.Set Beam.addressState addressState,
      Se.Set Beam.addressStreetAddress1 addressStreetAddress1,
      Se.Set Beam.addressStreetAddress2 addressStreetAddress2,
      Se.Set Beam.applicationId applicationId,
      Se.Set Beam.bankBranch bankBranch,
      Se.Set Beam.bankName bankName,
      Se.Set Beam.dateOfBirth dateOfBirth,
      Se.Set Beam.declarationDate declarationDate,
      Se.Set Beam.declarationPlace declarationPlace,
      Se.Set Beam.declarationSignature declarationSignature,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.emailId emailId,
      Se.Set Beam.fatherMotherName fatherMotherName,
      Se.Set Beam.firstName firstName,
      Se.Set Beam.fuelTypes fuelTypes,
      Se.Set Beam.ifscCodeEncrypted (((unEncrypted . (.encrypted) $ ifscCode))),
      Se.Set Beam.ifscCodeHash (((.hash) ifscCode)),
      Se.Set Beam.lastName lastName,
      Se.Set Beam.memberCategory memberCategory,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.mobileNumberEncrypted (((unEncrypted . (.encrypted) $ mobileNumber))),
      Se.Set Beam.mobileNumberHash (((.hash) mobileNumber)),
      Se.Set Beam.nomineeAadharEncrypted (((nomineeAadhar & unEncrypted . encrypted))),
      Se.Set Beam.nomineeAadharHash ((nomineeAadhar & hash)),
      Se.Set Beam.nomineeName nomineeName,
      Se.Set Beam.numberOfShares numberOfShares,
      Se.Set Beam.panNumberEncrypted (((unEncrypted . (.encrypted) $ panNumber))),
      Se.Set Beam.panNumberHash (((.hash) panNumber)),
      Se.Set Beam.status status,
      Se.Set Beam.termsAccepted termsAccepted,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleType vehicleType
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
