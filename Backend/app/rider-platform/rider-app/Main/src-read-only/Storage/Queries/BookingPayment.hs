{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BookingPayment (module Storage.Queries.BookingPayment, module ReExport) where

import qualified Domain.Types.BookingPayment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Sequelize as Se
import qualified Storage.Beam.BookingPayment as Beam
import Storage.Queries.BookingPaymentExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BookingPayment.BookingPayment -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BookingPayment.BookingPayment] -> m ())
createMany = traverse_ create

findAllByOrderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m ([Domain.Types.BookingPayment.BookingPayment]))
findAllByOrderId paymentOrderId = do findAllWithKV [Se.Is Beam.paymentOrderId $ Se.Eq (Kernel.Types.Id.getId paymentOrderId)]

updateStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BookingPayment.BookingPaymentStatus -> Kernel.Types.Id.Id Domain.Types.BookingPayment.BookingPayment -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BookingPayment.BookingPayment -> m (Maybe Domain.Types.BookingPayment.BookingPayment))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BookingPayment.BookingPayment -> m ())
updateByPrimaryKey (Domain.Types.BookingPayment.BookingPayment {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bookingId (Kernel.Types.Id.getId bookingId),
      Se.Set Beam.paymentOrderId (Kernel.Types.Id.getId paymentOrderId),
      Se.Set Beam.paymentServiceType paymentServiceType,
      Se.Set Beam.status status,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
