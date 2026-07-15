module Storage.Queries.IdfyVerificationExtra where

import qualified Domain.Types.DocumentVerificationConfig as DVC
import Domain.Types.Extra.IdfyVerification (docTypeToText)
import qualified Domain.Types.IdfyVerification as DIdfy
import qualified Domain.Types.Image as DImage
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.IdfyVerification as Beam
import Storage.Queries.IdfyVerification ()

findLatestByDriverIdAndDocType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Int ->
  Int ->
  Id DP.Person ->
  DVC.DocumentType ->
  UTCTime ->
  UTCTime ->
  m [DIdfy.IdfyVerification]
findLatestByDriverIdAndDocType limit offset driverId docType fromDate toDate = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is Beam.driverId $ Se.Eq (getId driverId)]
            <> [Se.Is Beam.docType $ Se.Eq (docTypeToText docType)]
            <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq fromDate]
            <> [Se.Is Beam.createdAt $ Se.LessThanOrEq toDate]
        )
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

-- | Latest still-pending (webhook not yet applied) verification row for a driver's document.
findLatestPendingByDriverIdAndDocType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  DVC.DocumentType ->
  m (Maybe DIdfy.IdfyVerification)
findLatestPendingByDriverIdAndDocType driverId docType =
  listToMaybe
    <$> findAllWithOptionsKV
      [ Se.And
          [ Se.Is Beam.driverId $ Se.Eq (getId driverId),
            Se.Is Beam.docType $ Se.Eq (docTypeToText docType),
            Se.Is Beam.status $ Se.In ["pending", "source_down_retrying"]
          ]
      ]
      (Se.Desc Beam.createdAt)
      (Just 1)
      Nothing

-- | Latest still-pending row for a specific document image (a fleet driver's RCs differ only by image).
findLatestPendingByDocTypeAndImage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  DVC.DocumentType ->
  Id DImage.Image ->
  m (Maybe DIdfy.IdfyVerification)
findLatestPendingByDocTypeAndImage driverId docType imageId =
  listToMaybe
    <$> findAllWithOptionsKV
      [ Se.And
          [ Se.Is Beam.driverId $ Se.Eq (getId driverId),
            Se.Is Beam.docType $ Se.Eq (docTypeToText docType),
            Se.Is Beam.documentImageId1 $ Se.Eq (getId imageId),
            Se.Is Beam.status $ Se.In ["pending", "source_down_retrying"]
          ]
      ]
      (Se.Desc Beam.createdAt)
      (Just 1)
      Nothing

findLatestCompletedByDriverIdAndDocType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  DVC.DocumentType ->
  m (Maybe DIdfy.IdfyVerification)
findLatestCompletedByDriverIdAndDocType driverId docType =
  listToMaybe
    <$> findAllWithOptionsKV
      [ Se.And
          [ Se.Is Beam.driverId $ Se.Eq (getId driverId),
            Se.Is Beam.docType $ Se.Eq (docTypeToText docType),
            Se.Is Beam.status $ Se.Eq "completed"
          ]
      ]
      (Se.Desc Beam.createdAt)
      (Just 1)
      Nothing

-- | Latest still-pending row by document number hash — lets the no-driver-context /vehicleStatus path
--   resolve the row (and its driverId) from a registration number.
findLatestPendingByDocNumberHashAndDocType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DbHash ->
  DVC.DocumentType ->
  m (Maybe DIdfy.IdfyVerification)
findLatestPendingByDocNumberHashAndDocType docNumberHash docType =
  listToMaybe
    <$> findAllWithOptionsKV
      [ Se.And
          [ Se.Is Beam.documentNumberHash $ Se.Eq docNumberHash,
            Se.Is Beam.docType $ Se.Eq (docTypeToText docType),
            Se.Is Beam.status $ Se.In ["pending", "source_down_retrying"]
          ]
      ]
      (Se.Desc Beam.createdAt)
      (Just 1)
      Nothing
