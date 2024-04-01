{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PersonDefaultEmergencyNumber where

import qualified Domain.Types.Person
import qualified Domain.Types.PersonDefaultEmergencyNumber
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PersonDefaultEmergencyNumber as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber] -> m ())
createMany = traverse_ create

deleteAllByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteAllByPersonId (Kernel.Types.Id.Id personId) = do deleteWithKV [Se.Is Beam.personId $ Se.Eq personId]

findAllByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber])
findAllByPersonId (Kernel.Types.Id.Id personId) = do findAllWithKV [Se.Is Beam.personId $ Se.Eq personId]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber))
findByPrimaryKey mobileCountryCode (Kernel.Types.Id.Id personId) = do findOneWithKV [Se.And [Se.Is Beam.mobileCountryCode $ Se.Eq mobileCountryCode, Se.Is Beam.personId $ Se.Eq personId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber -> m ())
updateByPrimaryKey (Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.mobileNumberEncrypted (mobileNumber & unEncrypted . encrypted),
      Se.Set Beam.mobileNumberHash (mobileNumber & hash),
      Se.Set Beam.name name,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.mobileCountryCode $ Se.Eq mobileCountryCode, Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

instance FromTType' Beam.PersonDefaultEmergencyNumber Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber where
  fromTType' (Beam.PersonDefaultEmergencyNumberT {..}) = do
    pure $
      Just
        Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber
          { merchantId = Kernel.Types.Id.Id merchantId,
            mobileCountryCode = mobileCountryCode,
            mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
            name = name,
            personId = Kernel.Types.Id.Id personId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PersonDefaultEmergencyNumber Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber where
  toTType' (Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber {..}) = do
    Beam.PersonDefaultEmergencyNumberT
      { Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.mobileCountryCode = mobileCountryCode,
        Beam.mobileNumberEncrypted = mobileNumber & unEncrypted . encrypted,
        Beam.mobileNumberHash = mobileNumber & hash,
        Beam.name = name,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
