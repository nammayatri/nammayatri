module Storage.Queries.DriverSSNExtra where

import Domain.Types.DriverSSN
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Documents
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverSSN as Beam
import Storage.Queries.OrphanInstances.DriverSSN ()

-- Extra code goes here --

upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => (Domain.Types.DriverSSN.DriverSSN -> m ())
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

findBySSN :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) => (Kernel.External.Encryption.DbHash -> m (Maybe Domain.Types.DriverSSN.DriverSSN))
findBySSN ssn = do findOneWithKV [Se.Is Beam.ssnHash $ Se.Eq ssn]

updateVerificationStatusAndReasonBySSN ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Documents.VerificationStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.External.Encryption.DbHash -> m ())
updateVerificationStatusAndReasonBySSN verificationStatus rejectReason ssn = do
  updateOneWithKV
    [ Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.rejectReason rejectReason
    ]
    [Se.Is Beam.ssnHash $ Se.Eq ssn]
