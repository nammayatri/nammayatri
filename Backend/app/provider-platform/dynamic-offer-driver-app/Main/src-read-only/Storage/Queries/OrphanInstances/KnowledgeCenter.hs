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
            fileType = fileType,
            id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            s3Path = s3Path,
            sopType = sopType,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId
          }

instance ToTType' Beam.KnowledgeCenter Domain.Types.KnowledgeCenter.KnowledgeCenter where
  toTType' (Domain.Types.KnowledgeCenter.KnowledgeCenter {..}) = do
    Beam.KnowledgeCenterT
      { Beam.createdAt = createdAt,
        Beam.fileType = fileType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.s3Path = s3Path,
        Beam.sopType = sopType,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId
      }
