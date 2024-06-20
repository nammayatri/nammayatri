{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Issue where

import qualified Domain.Types.Issue
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Issue as Beam

instance FromTType' Beam.Issue Domain.Types.Issue.Issue where
  fromTType' (Beam.IssueT {..}) = do
    pure $
      Just
        Domain.Types.Issue.Issue
          { id = Kernel.Types.Id.Id id,
            customerId = Kernel.Types.Id.Id customerId,
            bookingId = Kernel.Types.Id.Id <$> bookingId,
            contactEmail = contactEmail,
            reason = reason,
            description = description,
            ticketId = ticketId,
            status = status,
            nightSafety = nightSafety,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Issue Domain.Types.Issue.Issue where
  toTType' (Domain.Types.Issue.Issue {..}) = do
    Beam.IssueT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.customerId = Kernel.Types.Id.getId customerId,
        Beam.bookingId = Kernel.Types.Id.getId <$> bookingId,
        Beam.contactEmail = contactEmail,
        Beam.reason = reason,
        Beam.description = description,
        Beam.ticketId = ticketId,
        Beam.status = status,
        Beam.nightSafety = nightSafety,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
