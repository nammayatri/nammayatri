{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverPanCard (module Storage.Queries.DriverPanCard, module ReExport) where

import qualified Domain.Types.DriverPanCard
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverPanCard as Beam
import Storage.Queries.DriverPanCardExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.DriverPanCard.DriverPanCard -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.DriverPanCard.DriverPanCard] -> m ())
createMany = traverse_ create

deleteByDriverId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByDriverId (Kernel.Types.Id.Id driverId) = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findByDriverId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverPanCard.DriverPanCard))
findByDriverId (Kernel.Types.Id.Id driverId) = do findOneWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.DriverPanCard.DriverPanCard -> m (Maybe Domain.Types.DriverPanCard.DriverPanCard))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.DriverPanCard.DriverPanCard -> m (Maybe Domain.Types.DriverPanCard.DriverPanCard))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.DriverPanCard.DriverPanCard -> m ())
updateByPrimaryKey (Domain.Types.DriverPanCard.DriverPanCard {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.consent consent,
      Se.Set Beam.consentTimestamp consentTimestamp,
      Se.Set Beam.documentImageId1 (Kernel.Types.Id.getId documentImageId1),
      Se.Set Beam.documentImageId2 (Kernel.Types.Id.getId <$> documentImageId2),
      Se.Set Beam.driverDob driverDob,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.driverName driverName,
      Se.Set Beam.failedRules failedRules,
      Se.Set Beam.panCardNumberEncrypted (panCardNumber & unEncrypted . encrypted),
      Se.Set Beam.panCardNumberHash (panCardNumber & hash),
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
