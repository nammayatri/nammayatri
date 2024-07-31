{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicketBookingExtra where

import Domain.Types.FRFSTicketBooking
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBooking as Beam
import Storage.Queries.OrphanInstances.FRFSTicketBooking

-- Extra code goes here --

insertPayerVpaIfNotPresent :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe Text -> Id FRFSTicketBooking -> m ()
insertPayerVpaIfNotPresent (Just vpa) bookingId = do
  mbBooking <- findOneWithKV [Se.Is Beam.id $ Se.Eq (bookingId.getId)]
  whenJust mbBooking $ \booking -> do
    when (isNothing booking.payerVpa) $ do
      updatePayerVpaByBookingId bookingId vpa
  pure ()
insertPayerVpaIfNotPresent _ _ = pure ()

updatePayerVpaByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id FRFSTicketBooking -> Text -> m ()
updatePayerVpaByBookingId id payerVpa = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.payerVpa (Just payerVpa), Se.Set Beam.updatedAt now] [Se.Is Beam.id $ Se.Eq (id.getId)]
