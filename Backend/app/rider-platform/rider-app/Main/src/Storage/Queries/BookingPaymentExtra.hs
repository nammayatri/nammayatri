module Storage.Queries.BookingPaymentExtra where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingPayment as DBP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Sequelize as Se
import qualified Storage.Beam.BookingPayment as Beam
import Storage.Queries.OrphanInstances.BookingPayment ()

findLatestByBookingIdAndServiceType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DRB.Booking -> DOrder.PaymentServiceType -> m (Maybe DBP.BookingPayment)
findLatestByBookingIdAndServiceType (Id bookingId) serviceType =
  findAllWithOptionsKV [Se.And [Se.Is Beam.bookingId $ Se.Eq bookingId, Se.Is Beam.paymentServiceType $ Se.Eq serviceType]] (Se.Desc Beam.createdAt) (Just 1) Nothing <&> listToMaybe

findAllByBookingIdAndServiceType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DRB.Booking -> DOrder.PaymentServiceType -> m [DBP.BookingPayment]
findAllByBookingIdAndServiceType (Id bookingId) serviceType =
  findAllWithOptionsKV [Se.And [Se.Is Beam.bookingId $ Se.Eq bookingId, Se.Is Beam.paymentServiceType $ Se.Eq serviceType]] (Se.Desc Beam.createdAt) Nothing Nothing
