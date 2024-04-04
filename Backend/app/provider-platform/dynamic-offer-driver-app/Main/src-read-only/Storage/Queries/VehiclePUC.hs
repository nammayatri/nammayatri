{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehiclePUC where

import qualified Domain.Types.Person
import qualified Domain.Types.VehiclePUC
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehiclePUC as Beam

create :: KvDbFlow m r => (Domain.Types.VehiclePUC.VehiclePUC -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.VehiclePUC.VehiclePUC] -> m ())
createMany = traverse_ create

findByRcIdAndDriverId ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.VehiclePUC.VehiclePUC])
findByRcIdAndDriverId (Kernel.Types.Id.Id rcId) (Kernel.Types.Id.Id driverId) = do findAllWithKV [Se.And [Se.Is Beam.rcId $ Se.Eq rcId, Se.Is Beam.driverId $ Se.Eq driverId]]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.VehiclePUC.VehiclePUC -> m (Maybe Domain.Types.VehiclePUC.VehiclePUC))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.VehiclePUC.VehiclePUC -> m ())
updateByPrimaryKey (Domain.Types.VehiclePUC.VehiclePUC {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.documentImageId (Kernel.Types.Id.getId documentImageId),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.pucExpiry pucExpiry,
      Se.Set Beam.rcId (Kernel.Types.Id.getId rcId),
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.VehiclePUC Domain.Types.VehiclePUC.VehiclePUC where
  fromTType' (Beam.VehiclePUCT {..}) = do
    pure $
      Just
        Domain.Types.VehiclePUC.VehiclePUC
          { documentImageId = Kernel.Types.Id.Id documentImageId,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            pucExpiry = pucExpiry,
            rcId = Kernel.Types.Id.Id rcId,
            verificationStatus = verificationStatus,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VehiclePUC Domain.Types.VehiclePUC.VehiclePUC where
  toTType' (Domain.Types.VehiclePUC.VehiclePUC {..}) = do
    Beam.VehiclePUCT
      { Beam.documentImageId = Kernel.Types.Id.getId documentImageId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.pucExpiry = pucExpiry,
        Beam.rcId = Kernel.Types.Id.getId rcId,
        Beam.verificationStatus = verificationStatus,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
