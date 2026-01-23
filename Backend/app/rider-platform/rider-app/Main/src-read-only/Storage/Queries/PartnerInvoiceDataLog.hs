{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PartnerInvoiceDataLog where

import qualified Domain.Types.Booking
import qualified Domain.Types.PartnerInvoiceDataLog
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PartnerInvoiceDataLog as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PartnerInvoiceDataLog.PartnerInvoiceDataLog -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PartnerInvoiceDataLog.PartnerInvoiceDataLog] -> m ())
createMany = traverse_ create

findByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m (Maybe Domain.Types.PartnerInvoiceDataLog.PartnerInvoiceDataLog))
findByBookingId bookingId = do findOneWithKV [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PartnerInvoiceDataLog.PartnerInvoiceDataLog -> m (Maybe Domain.Types.PartnerInvoiceDataLog.PartnerInvoiceDataLog))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findUnexportedSince :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.UTCTime -> m ([Domain.Types.PartnerInvoiceDataLog.PartnerInvoiceDataLog]))
findUnexportedSince requestedAt = do findAllWithKV [Se.And [Se.Is Beam.exportedAt $ Se.Eq Nothing, Se.Is Beam.requestedAt $ Se.GreaterThanOrEq requestedAt]]

markAsExported :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.PartnerInvoiceDataLog.PartnerInvoiceDataLog -> m ())
markAsExported exportedAt id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.exportedAt exportedAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

instance FromTType' Beam.PartnerInvoiceDataLog Domain.Types.PartnerInvoiceDataLog.PartnerInvoiceDataLog where
  fromTType' (Beam.PartnerInvoiceDataLogT {..}) = do
    pure $
      Just
        Domain.Types.PartnerInvoiceDataLog.PartnerInvoiceDataLog
          { bookingId = Kernel.Types.Id.Id bookingId,
            createdAt = createdAt,
            exportedAt = exportedAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            requestedAt = requestedAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PartnerInvoiceDataLog Domain.Types.PartnerInvoiceDataLog.PartnerInvoiceDataLog where
  toTType' (Domain.Types.PartnerInvoiceDataLog.PartnerInvoiceDataLog {..}) = do
    Beam.PartnerInvoiceDataLogT
      { Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.createdAt = createdAt,
        Beam.exportedAt = exportedAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.requestedAt = requestedAt,
        Beam.updatedAt = updatedAt
      }
