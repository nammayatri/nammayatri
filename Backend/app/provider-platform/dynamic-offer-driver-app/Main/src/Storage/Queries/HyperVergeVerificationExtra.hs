module Storage.Queries.HyperVergeVerificationExtra where

import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.HyperVergeVerification
import qualified Domain.Types.Image as DImage
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.HyperVergeVerification as Beam
import Storage.Queries.HyperVergeVerification ()

findLatestByDriverIdAndDocType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Int ->
  Int ->
  Id DP.Person ->
  DVC.DocumentType ->
  UTCTime ->
  UTCTime ->
  m [Domain.Types.HyperVergeVerification.HyperVergeVerification]
findLatestByDriverIdAndDocType limit offset driverId docType fromDate toDate = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is Beam.driverId $ Se.Eq (getId driverId)]
            <> [Se.Is Beam.docType $ Se.Eq docType]
            <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq (fromDate)]
            <> [Se.Is Beam.createdAt $ Se.LessThanOrEq (toDate)]
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
  m (Maybe Domain.Types.HyperVergeVerification.HyperVergeVerification)
findLatestPendingByDriverIdAndDocType driverId docType =
  listToMaybe
    <$> findAllWithOptionsKV
      [ Se.And
          [ Se.Is Beam.driverId $ Se.Eq (getId driverId),
            Se.Is Beam.docType $ Se.Eq docType,
            Se.Is Beam.status $ Se.In ["pending", "source_down_retrying"]
          ]
      ]
      (Se.Desc Beam.createdAt)
      (Just 1)
      Nothing

-- | Latest still-pending (webhook not yet applied) verification row for a specific document image.
findLatestPendingByDocTypeAndImage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  DVC.DocumentType ->
  Id DImage.Image ->
  m (Maybe Domain.Types.HyperVergeVerification.HyperVergeVerification)
findLatestPendingByDocTypeAndImage driverId docType imageId =
  listToMaybe
    <$> findAllWithOptionsKV
      [ Se.And
          [ Se.Is Beam.driverId $ Se.Eq (getId driverId),
            Se.Is Beam.docType $ Se.Eq docType,
            Se.Is Beam.documentImageId1 $ Se.Eq (getId imageId),
            Se.Is Beam.status $ Se.In ["pending", "source_down_retrying"]
          ]
      ]
      (Se.Desc Beam.createdAt)
      (Just 1)
      Nothing

-- | HyperVerge counterpart of the Idfy by-document-number query.
findLatestPendingByDocNumberHashAndDocType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DbHash ->
  DVC.DocumentType ->
  m (Maybe Domain.Types.HyperVergeVerification.HyperVergeVerification)
findLatestPendingByDocNumberHashAndDocType docNumberHash docType =
  listToMaybe
    <$> findAllWithOptionsKV
      [ Se.And
          [ Se.Is Beam.documentNumberHash $ Se.Eq docNumberHash,
            Se.Is Beam.docType $ Se.Eq docType,
            Se.Is Beam.status $ Se.In ["pending", "source_down_retrying"]
          ]
      ]
      (Se.Desc Beam.createdAt)
      (Just 1)
      Nothing
