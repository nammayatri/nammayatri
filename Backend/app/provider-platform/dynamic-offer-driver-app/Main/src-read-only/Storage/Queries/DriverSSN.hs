{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverSSN (module Storage.Queries.DriverSSN, module ReExport) where

import qualified Domain.Types.DriverSSN
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverSSN as Beam
import Storage.Queries.DriverSSNExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.DriverSSN.DriverSSN -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.DriverSSN.DriverSSN] -> m ())
createMany = traverse_ create

findByDriverId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverSSN.DriverSSN))
findByDriverId (Kernel.Types.Id.Id driverId) = do findOneWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.DriverSSN.DriverSSN -> m (Maybe Domain.Types.DriverSSN.DriverSSN))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.DriverSSN.DriverSSN -> m ())
updateByPrimaryKey (Domain.Types.DriverSSN.DriverSSN {..}) = do
  updateWithKV
    [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.rejectReason rejectReason,
      Se.Set Beam.ssnEncrypted (ssn & unEncrypted . encrypted),
      Se.Set Beam.ssnHash (ssn & hash),
      Se.Set Beam.verificationStatus verificationStatus
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
