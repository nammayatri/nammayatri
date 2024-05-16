{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverSSNExtra where

import Domain.Types.DriverSSN
import Domain.Types.IdfyVerification
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverSSN as Beam
import Storage.Queries.OrphanInstances.DriverSSN

-- Extra code goes here --

upsert :: KvDbFlow m r => (Domain.Types.DriverSSN.DriverSSN -> m ())
upsert driverSsn@(Domain.Types.DriverSSN.DriverSSN {..}) = do
  res <- findOneWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
  if isJust res
    then
      updateWithKV
        [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
          Se.Set Beam.ssnEncrypted (ssn & unEncrypted . encrypted),
          Se.Set Beam.ssnHash (ssn & hash),
          Se.Set Beam.verificationStatus verificationStatus
        ]
        [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]
    else createWithKV driverSsn

findBySSN :: (KvDbFlow m r, EncFlow m r) => (Kernel.External.Encryption.DbHash -> m (Maybe Domain.Types.DriverSSN.DriverSSN))
findBySSN ssn = do findOneWithKV [Se.Is Beam.ssnHash $ Se.Eq ssn]

updateVerificationStatusAndReasonBySSN ::
  KvDbFlow m r =>
  (Domain.Types.IdfyVerification.VerificationStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.External.Encryption.DbHash -> m ())
updateVerificationStatusAndReasonBySSN verificationStatus rejectReason ssn = do
  updateOneWithKV
    [ Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.rejectReason rejectReason
    ]
    [Se.Is Beam.ssnHash $ Se.Eq ssn]
