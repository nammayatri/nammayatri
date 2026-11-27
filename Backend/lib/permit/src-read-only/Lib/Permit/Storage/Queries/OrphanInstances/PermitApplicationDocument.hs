{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Permit.Storage.Queries.OrphanInstances.PermitApplicationDocument where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Permit.Domain.Types.PermitApplicationDocument
import qualified Lib.Permit.Storage.Beam.PermitApplicationDocument as Beam

instance FromTType' Beam.PermitApplicationDocument Lib.Permit.Domain.Types.PermitApplicationDocument.PermitApplicationDocument where
  fromTType' (Beam.PermitApplicationDocumentT {..}) = do
    pure $
      Just
        Lib.Permit.Domain.Types.PermitApplicationDocument.PermitApplicationDocument
          { applicationId = Kernel.Types.Id.Id applicationId,
            contentType = contentType,
            createdAt = createdAt,
            documentType = documentType,
            fileHash = fileHash,
            id = Kernel.Types.Id.Id id,
            lifetime = lifetime,
            s3Path = s3Path,
            sizeBytes = sizeBytes,
            updatedAt = updatedAt,
            validUntil = validUntil
          }

instance ToTType' Beam.PermitApplicationDocument Lib.Permit.Domain.Types.PermitApplicationDocument.PermitApplicationDocument where
  toTType' (Lib.Permit.Domain.Types.PermitApplicationDocument.PermitApplicationDocument {..}) = do
    Beam.PermitApplicationDocumentT
      { Beam.applicationId = Kernel.Types.Id.getId applicationId,
        Beam.contentType = contentType,
        Beam.createdAt = createdAt,
        Beam.documentType = documentType,
        Beam.fileHash = fileHash,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lifetime = lifetime,
        Beam.s3Path = s3Path,
        Beam.sizeBytes = sizeBytes,
        Beam.updatedAt = updatedAt,
        Beam.validUntil = validUntil
      }
