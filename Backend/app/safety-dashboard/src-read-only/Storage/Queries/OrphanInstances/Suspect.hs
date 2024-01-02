{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Suspect where

import qualified Domain.Types.Suspect
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Suspect as Beam
import Storage.Queries.Transformers.Suspect

instance FromTType' Beam.Suspect Domain.Types.Suspect.Suspect where
  fromTType' (Beam.SuspectT {..}) = do
    flaggedBy' <- getFlaggedByFromTable flaggedBy
    pure $
      Just
        Domain.Types.Suspect.Suspect
          { createdAt = createdAt,
            dl = dl,
            firstName = firstName,
            flagUpdatedAt = flagUpdatedAt,
            flaggedBy = flaggedBy',
            flaggedCounter = flaggedCounter,
            flaggedStatus = flaggedStatus,
            id = Kernel.Types.Id.Id id,
            lastName = lastName,
            statusChangedReason = statusChangedReason,
            updatedAt = updatedAt,
            voterId = voterId
          }

instance ToTType' Beam.Suspect Domain.Types.Suspect.Suspect where
  toTType' (Domain.Types.Suspect.Suspect {..}) = do
    Beam.SuspectT
      { Beam.createdAt = createdAt,
        Beam.dl = dl,
        Beam.firstName = firstName,
        Beam.flagUpdatedAt = flagUpdatedAt,
        Beam.flaggedBy = convertFlaggedByToTable flaggedBy,
        Beam.flaggedCounter = flaggedCounter,
        Beam.flaggedStatus = flaggedStatus,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastName = lastName,
        Beam.statusChangedReason = statusChangedReason,
        Beam.updatedAt = updatedAt,
        Beam.voterId = voterId
      }
