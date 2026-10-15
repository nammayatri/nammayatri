{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BookingPartiesLink (module Storage.Queries.BookingPartiesLink, module ReExport) where

import qualified Domain.Types.Booking
import qualified Domain.Types.BookingPartiesLink
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BookingPartiesLink as Beam
import Storage.Queries.BookingPartiesLinkExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BookingPartiesLink.BookingPartiesLink -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BookingPartiesLink.BookingPartiesLink] -> m ())
createMany = traverse_ create

findAllActiveByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m [Domain.Types.BookingPartiesLink.BookingPartiesLink])
findAllActiveByBookingId bookingId = do findAllWithKVAndConditionalDB [Se.And [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId), Se.Is Beam.isActive $ Se.Eq True]] Nothing

findAllByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m [Domain.Types.BookingPartiesLink.BookingPartiesLink])
findAllByBookingId bookingId = do findAllWithKVAndConditionalDB [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)] Nothing

makeAllInactiveByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m ())
makeAllInactiveByBookingId bookingId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.isActive False, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId), Se.Is Beam.isActive $ Se.Eq True]]
