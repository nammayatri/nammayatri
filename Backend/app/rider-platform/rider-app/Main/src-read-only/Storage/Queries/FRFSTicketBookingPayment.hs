{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicketBookingPayment where

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

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment] -> m ())
createMany = traverse_ create

findAllByStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPaymentStatus -> m ([Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment]))
findAllByStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

findAllTicketBookingId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ([Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment]))
findAllTicketBookingId frfsTicketBookingId = do findAllWithKV [Se.Is Beam.frfsTicketBookingId $ Se.Eq (Kernel.Types.Id.getId frfsTicketBookingId)]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment -> m (Maybe Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPaymentOrderId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m (Maybe Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment))
findByPaymentOrderId paymentOrderId = do findOneWithKV [Se.Is Beam.paymentOrderId $ Se.Eq (Kernel.Types.Id.getId paymentOrderId)]

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
    [ Se.Set Beam.frfsTicketBookingId (Kernel.Types.Id.getId frfsTicketBookingId),
      Se.Set Beam.paymentOrderId (Kernel.Types.Id.getId paymentOrderId),
      Se.Set Beam.status status,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSTicketBookingPayment Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment where
  fromTType' (Beam.FRFSTicketBookingPaymentT {..}) = do
    pure $
      Just
        Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment
          { frfsTicketBookingId = Kernel.Types.Id.Id frfsTicketBookingId,
            id = Kernel.Types.Id.Id id,
            paymentOrderId = Kernel.Types.Id.Id paymentOrderId,
            status = status,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSTicketBookingPayment Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment where
  toTType' (Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment {..}) = do
    Beam.FRFSTicketBookingPaymentT
      { Beam.frfsTicketBookingId = Kernel.Types.Id.getId frfsTicketBookingId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.paymentOrderId = Kernel.Types.Id.getId paymentOrderId,
        Beam.status = status,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
