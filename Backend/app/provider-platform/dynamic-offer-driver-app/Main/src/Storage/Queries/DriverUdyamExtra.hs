module Storage.Queries.DriverUdyamExtra where

import Domain.Types.DriverUdyam
import qualified Domain.Types.Image
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverUdyam as Beam
import Storage.Queries.DriverUdyam ()

upsert :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DriverUdyam -> m ()
upsert a@DriverUdyam {..} =
  findOneWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)] >>= \case
    Just _ ->
      updateOneWithKV
        [ Se.Set Beam.documentImageId (Kernel.Types.Id.getId documentImageId),
          Se.Set Beam.udyamNumberEncrypted (udyamNumber & unEncrypted . encrypted),
          Se.Set Beam.udyamNumberHash (udyamNumber & hash),
          Se.Set Beam.verificationStatus verificationStatus,
          Se.Set Beam.rejectReason rejectReason,
          Se.Set Beam.verifiedBy verifiedBy,
          Se.Set Beam.updatedAt updatedAt
        ]
        [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
    Nothing -> createWithKV a

deleteByDriverIdAndStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> Documents.VerificationStatus -> m ()
deleteByDriverIdAndStatus driverId status =
  deleteWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.verificationStatus $ Se.Eq status]]

-- | Apply a verification result: sets only the columns that result owns. The caller reads the row to
--   decide create-vs-update, so a full-row updateByPrimaryKey would write every other column (notably
--   udyamNumber) back from that earlier read and revert anything written since. Keyed by the row's own
--   id (the caller has it), so it targets exactly the row updateByPrimaryKey did.
updateVerificationResultById ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Documents.VerificationStatus ->
  Kernel.Types.Id.Id Domain.Types.Image.Image ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Types.Id.Id DriverUdyam ->
  m ()
updateVerificationResultById verificationStatus documentImageId enterpriseName enterpriseType udyamId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.documentImageId (Kernel.Types.Id.getId documentImageId),
      Se.Set Beam.enterpriseName enterpriseName,
      Se.Set Beam.enterpriseType enterpriseType,
      Se.Set Beam.rejectReason Nothing,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId udyamId)]
