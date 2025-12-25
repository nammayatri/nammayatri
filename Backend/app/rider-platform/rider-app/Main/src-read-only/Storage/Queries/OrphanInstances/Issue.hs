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
          { bookingId = Kernel.Types.Id.Id <$> bookingId,
            contactEmail = contactEmail,
            createdAt = createdAt,
            customerId = Kernel.Types.Id.Id customerId,
            description = description,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            nightSafety = nightSafety,
            reason = reason,
            status = status,
            ticketId = ticketId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Issue Domain.Types.Issue.Issue where
  toTType' (Domain.Types.Issue.Issue {..}) = do
    Beam.IssueT
      { Beam.bookingId = Kernel.Types.Id.getId <$> bookingId,
        Beam.contactEmail = contactEmail,
        Beam.createdAt = createdAt,
        Beam.customerId = Kernel.Types.Id.getId customerId,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.nightSafety = nightSafety,
        Beam.reason = reason,
        Beam.status = status,
        Beam.ticketId = ticketId,
        Beam.updatedAt = updatedAt
      }
