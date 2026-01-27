{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverRidePayoutBankAccount where

import qualified Domain.Types.DriverRidePayoutBankAccount
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverRidePayoutBankAccount as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverRidePayoutBankAccount.DriverRidePayoutBankAccount -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverRidePayoutBankAccount.DriverRidePayoutBankAccount] -> m ())
createMany = traverse_ create

findAllByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.DriverRidePayoutBankAccount.DriverRidePayoutBankAccount])
findAllByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByRcId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m (Maybe Domain.Types.DriverRidePayoutBankAccount.DriverRidePayoutBankAccount))
findByRcId rcId = do findOneWithKV [Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DriverRidePayoutBankAccount.DriverRidePayoutBankAccount -> m (Maybe Domain.Types.DriverRidePayoutBankAccount.DriverRidePayoutBankAccount))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverRidePayoutBankAccount.DriverRidePayoutBankAccount -> m ())
updateByPrimaryKey (Domain.Types.DriverRidePayoutBankAccount.DriverRidePayoutBankAccount {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bankAccountNumberEncrypted (bankAccountNumber <&> unEncrypted . (.encrypted)),
      Se.Set Beam.bankAccountNumberHash (bankAccountNumber <&> (.hash)),
      Se.Set Beam.bankIfscCodeEncrypted (bankIfscCode <&> unEncrypted . (.encrypted)),
      Se.Set Beam.bankIfscCodeHash (bankIfscCode <&> (.hash)),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.rcId (Kernel.Types.Id.getId rcId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DriverRidePayoutBankAccount Domain.Types.DriverRidePayoutBankAccount.DriverRidePayoutBankAccount where
  fromTType' (Beam.DriverRidePayoutBankAccountT {..}) = do
    pure $
      Just
        Domain.Types.DriverRidePayoutBankAccount.DriverRidePayoutBankAccount
          { bankAccountNumber = EncryptedHashed <$> (Encrypted <$> bankAccountNumberEncrypted) <*> bankAccountNumberHash,
            bankIfscCode = EncryptedHashed <$> (Encrypted <$> bankIfscCodeEncrypted) <*> bankIfscCodeHash,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            rcId = Kernel.Types.Id.Id rcId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverRidePayoutBankAccount Domain.Types.DriverRidePayoutBankAccount.DriverRidePayoutBankAccount where
  toTType' (Domain.Types.DriverRidePayoutBankAccount.DriverRidePayoutBankAccount {..}) = do
    Beam.DriverRidePayoutBankAccountT
      { Beam.bankAccountNumberEncrypted = bankAccountNumber <&> unEncrypted . (.encrypted),
        Beam.bankAccountNumberHash = bankAccountNumber <&> (.hash),
        Beam.bankIfscCodeEncrypted = bankIfscCode <&> unEncrypted . (.encrypted),
        Beam.bankIfscCodeHash = bankIfscCode <&> (.hash),
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.rcId = Kernel.Types.Id.getId rcId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
