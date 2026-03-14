{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.KnowledgeCenter where

import qualified Domain.Types.KnowledgeCenter
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.KnowledgeCenter as Beam

instance FromTType' Beam.KnowledgeCenter Domain.Types.KnowledgeCenter.KnowledgeCenter where
  fromTType' (Beam.KnowledgeCenterT {..}) = do
    pure $
      Just
        Domain.Types.KnowledgeCenter.KnowledgeCenter
          { createdAt = createdAt,
            documentName = documentName,
            fileType = fileType,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            s3Path = s3Path,
            sopType = sopType,
            updatedAt = updatedAt
          }

instance ToTType' Beam.KnowledgeCenter Domain.Types.KnowledgeCenter.KnowledgeCenter where
  toTType' (Domain.Types.KnowledgeCenter.KnowledgeCenter {..}) = do
    Beam.KnowledgeCenterT
      { Beam.createdAt = createdAt,
        Beam.documentName = documentName,
        Beam.fileType = fileType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.s3Path = s3Path,
        Beam.sopType = sopType,
        Beam.updatedAt = updatedAt
      }
