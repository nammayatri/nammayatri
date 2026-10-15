{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicketBookingPayment (module Storage.Queries.FRFSTicketBookingPayment, module ReExport) where

import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBookingPayment as Beam
import Storage.Queries.FRFSTicketBookingPaymentExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment] -> m ())
createMany = traverse_ create

findAllByOrderId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m [Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment])
findAllByOrderId paymentOrderId = do findAllWithKV [Se.Is Beam.paymentOrderId $ Se.Eq (Kernel.Types.Id.getId paymentOrderId)]

findAllByStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPaymentStatus -> m [Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment])
findAllByStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment -> m (Maybe Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPaymentStatus -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusByTicketBookingId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPaymentStatus -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateStatusByTicketBookingId status frfsTicketBookingId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.frfsTicketBookingId $ Se.Eq (Kernel.Types.Id.getId frfsTicketBookingId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment -> m (Maybe Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment -> m ())
updateByPrimaryKey (Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.frfsQuoteId (Kernel.Types.Id.getId <$> frfsQuoteId),
      Se.Set Beam.frfsTicketBookingId (Kernel.Types.Id.getId frfsTicketBookingId),
      Se.Set Beam.paymentOrderId (Kernel.Types.Id.getId paymentOrderId),
      Se.Set Beam.status status,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
