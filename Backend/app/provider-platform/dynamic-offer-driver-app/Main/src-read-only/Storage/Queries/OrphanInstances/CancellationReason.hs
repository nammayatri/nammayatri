{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CancellationReason where

import qualified Data.Text
import qualified Domain.Types.CancellationReason
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CancellationReason as Beam
import Storage.Queries.Transformers.CancellationReason

instance FromTType' Beam.CancellationReason Domain.Types.CancellationReason.CancellationReason where
  fromTType' Beam.CancellationReasonT {..} = do
    pure $
      Just
        Domain.Types.CancellationReason.CancellationReason
          { description = description,
            enabled = enabled,
            priority = priority,
            reasonCode = reasonCode,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CancellationReason Domain.Types.CancellationReason.CancellationReason where
  toTType' Domain.Types.CancellationReason.CancellationReason {..} = do
    Beam.CancellationReasonT
      { Beam.description = description,
        Beam.enabled = enabled,
        Beam.priority = priority,
        Beam.reasonCode = reasonCodeToText reasonCode,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
