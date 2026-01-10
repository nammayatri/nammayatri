{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PaymentInvoice where

import qualified Domain.Types.PaymentInvoice
import qualified Domain.Types.Ride
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Sequelize as Se
import qualified Storage.Beam.PaymentInvoice as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PaymentInvoice.PaymentInvoice -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PaymentInvoice.PaymentInvoice] -> m ())
createMany = traverse_ create

findAllByRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ([Domain.Types.PaymentInvoice.PaymentInvoice]))
findAllByRideId rideId = do findAllWithKV [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId rideId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PaymentInvoice.PaymentInvoice -> m (Maybe Domain.Types.PaymentInvoice.PaymentInvoice))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByInvoiceNumber :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.PaymentInvoice.PaymentInvoice))
findByInvoiceNumber invoiceNumber = do findOneWithKV [Se.Is Beam.invoiceNumber $ Se.Eq invoiceNumber]

findByPaymentOrderId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder) -> m ([Domain.Types.PaymentInvoice.PaymentInvoice]))
findByPaymentOrderId paymentOrderId = do findAllWithKV [Se.Is Beam.paymentOrderId $ Se.Eq (Kernel.Types.Id.getId <$> paymentOrderId)]

updatePaymentStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PaymentInvoice.InvoicePaymentStatus -> Kernel.Types.Id.Id Domain.Types.PaymentInvoice.PaymentInvoice -> m ())
updatePaymentStatus paymentStatus id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.paymentStatus paymentStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PaymentInvoice.PaymentInvoice -> m (Maybe Domain.Types.PaymentInvoice.PaymentInvoice))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PaymentInvoice.PaymentInvoice -> m ())
updateByPrimaryKey (Domain.Types.PaymentInvoice.PaymentInvoice {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.currency currency,
      Se.Set Beam.invoiceNumber invoiceNumber,
      Se.Set Beam.invoiceType invoiceType,
      Se.Set Beam.paymentMethod paymentMethod,
      Se.Set Beam.paymentOrderId (Kernel.Types.Id.getId <$> paymentOrderId),
      Se.Set Beam.paymentPurpose paymentPurpose,
      Se.Set Beam.paymentStatus paymentStatus,
      Se.Set Beam.rideId (Kernel.Types.Id.getId rideId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PaymentInvoice Domain.Types.PaymentInvoice.PaymentInvoice where
  fromTType' (Beam.PaymentInvoiceT {..}) = do
    pure $
      Just
        Domain.Types.PaymentInvoice.PaymentInvoice
          { amount = amount,
            createdAt = createdAt,
            currency = currency,
            id = Kernel.Types.Id.Id id,
            invoiceNumber = invoiceNumber,
            invoiceType = invoiceType,
            paymentMethod = paymentMethod,
            paymentOrderId = Kernel.Types.Id.Id <$> paymentOrderId,
            paymentPurpose = paymentPurpose,
            paymentStatus = paymentStatus,
            rideId = Kernel.Types.Id.Id rideId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PaymentInvoice Domain.Types.PaymentInvoice.PaymentInvoice where
  toTType' (Domain.Types.PaymentInvoice.PaymentInvoice {..}) = do
    Beam.PaymentInvoiceT
      { Beam.amount = amount,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.invoiceNumber = invoiceNumber,
        Beam.invoiceType = invoiceType,
        Beam.paymentMethod = paymentMethod,
        Beam.paymentOrderId = Kernel.Types.Id.getId <$> paymentOrderId,
        Beam.paymentPurpose = paymentPurpose,
        Beam.paymentStatus = paymentStatus,
        Beam.rideId = Kernel.Types.Id.getId rideId,
        Beam.updatedAt = updatedAt
      }
