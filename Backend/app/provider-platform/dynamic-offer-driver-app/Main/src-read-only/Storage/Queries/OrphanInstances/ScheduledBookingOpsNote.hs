{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.ScheduledBookingOpsNote where

import qualified Domain.Types.ScheduledBookingOpsNote
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.ScheduledBookingOpsNote as Beam

instance FromTType' Beam.ScheduledBookingOpsNote Domain.Types.ScheduledBookingOpsNote.ScheduledBookingOpsNote where
  fromTType' (Beam.ScheduledBookingOpsNoteT {..}) = do
    pure $
      Just
        Domain.Types.ScheduledBookingOpsNote.ScheduledBookingOpsNote
          { bookingId = Kernel.Types.Id.Id <$> bookingId,
            content = content,
            createdAt = createdAt,
            createdByDashboardUserId = createdByDashboardUserId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            noteType = noteType,
            status = status,
            transactionId = transactionId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ScheduledBookingOpsNote Domain.Types.ScheduledBookingOpsNote.ScheduledBookingOpsNote where
  toTType' (Domain.Types.ScheduledBookingOpsNote.ScheduledBookingOpsNote {..}) = do
    Beam.ScheduledBookingOpsNoteT
      { Beam.bookingId = Kernel.Types.Id.getId <$> bookingId,
        Beam.content = content,
        Beam.createdAt = createdAt,
        Beam.createdByDashboardUserId = createdByDashboardUserId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.noteType = noteType,
        Beam.status = status,
        Beam.transactionId = transactionId,
        Beam.updatedAt = updatedAt
      }
