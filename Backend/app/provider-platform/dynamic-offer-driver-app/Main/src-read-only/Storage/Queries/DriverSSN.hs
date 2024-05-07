{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverSSN where

import qualified Domain.Types.DriverSSN
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverSSN as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverSSN.DriverSSN -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverSSN.DriverSSN] -> m ())
createMany = traverse_ create

findByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverSSN.DriverSSN))
findByDriverId (Kernel.Types.Id.Id driverId) = do findOneWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverSSN.DriverSSN -> m (Maybe Domain.Types.DriverSSN.DriverSSN))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverSSN.DriverSSN -> m ())
updateByPrimaryKey (Domain.Types.DriverSSN.DriverSSN {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.ssnEncrypted (((ssn & unEncrypted . encrypted))),
      Se.Set Beam.ssnHash ((ssn & hash))
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DriverSSN Domain.Types.DriverSSN.DriverSSN where
  fromTType' (Beam.DriverSSNT {..}) = do
    pure $
      Just
        Domain.Types.DriverSSN.DriverSSN
          { driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            ssn = EncryptedHashed (Encrypted ssnEncrypted) ssnHash
          }

instance ToTType' Beam.DriverSSN Domain.Types.DriverSSN.DriverSSN where
  toTType' (Domain.Types.DriverSSN.DriverSSN {..}) = do
    Beam.DriverSSNT
      { Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.ssnEncrypted = ((ssn & unEncrypted . encrypted)),
        Beam.ssnHash = (ssn & hash)
      }
