{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Permit.Storage.Queries.PermitApplicationDocument (module Lib.Permit.Storage.Queries.PermitApplicationDocument, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Permit.Domain.Types.PermitApplication
import qualified Lib.Permit.Domain.Types.PermitApplicationDocument
import qualified Lib.Permit.Storage.Beam.BeamFlow
import qualified Lib.Permit.Storage.Beam.PermitApplicationDocument as Beam
import Lib.Permit.Storage.Queries.PermitApplicationDocumentExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Permit.Domain.Types.PermitApplicationDocument.PermitApplicationDocument -> m ())
create = createWithKV

createMany :: (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Permit.Domain.Types.PermitApplicationDocument.PermitApplicationDocument] -> m ())
createMany = traverse_ create

findAllByApplicationId ::
  (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> m ([Lib.Permit.Domain.Types.PermitApplicationDocument.PermitApplicationDocument]))
findAllByApplicationId applicationId = do findAllWithKV [Se.Is Beam.applicationId $ Se.Eq (Kernel.Types.Id.getId applicationId)]

findByPrimaryKey ::
  (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplicationDocument.PermitApplicationDocument -> m (Maybe Lib.Permit.Domain.Types.PermitApplicationDocument.PermitApplicationDocument))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Permit.Domain.Types.PermitApplicationDocument.PermitApplicationDocument -> m ())
updateByPrimaryKey (Lib.Permit.Domain.Types.PermitApplicationDocument.PermitApplicationDocument {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.applicationId (Kernel.Types.Id.getId applicationId),
      Se.Set Beam.contentType contentType,
      Se.Set Beam.documentType documentType,
      Se.Set Beam.fileHash fileHash,
      Se.Set Beam.lifetime lifetime,
      Se.Set Beam.s3Path s3Path,
      Se.Set Beam.sizeBytes sizeBytes,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validUntil validUntil
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
