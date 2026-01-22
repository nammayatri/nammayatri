{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Role where

import qualified Domain.Types.Role
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Role as Beam

instance FromTType' Beam.Role Domain.Types.Role.Role where
  fromTType' (Beam.RoleT {..}) = do
    pure $
      Just
        Domain.Types.Role.Role
          { createdAt = createdAt,
            description = description,
            id = Kernel.Types.Id.Id id,
            name = name,
            needsBppAccountCreation = needsBppAccountCreation,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Role Domain.Types.Role.Role where
  toTType' (Domain.Types.Role.Role {..}) = do
    Beam.RoleT
      { Beam.createdAt = createdAt,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.needsBppAccountCreation = needsBppAccountCreation,
        Beam.updatedAt = updatedAt
      }
