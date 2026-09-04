{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.BookingPayment where

import qualified Domain.Types.BookingPayment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.BookingPayment as Beam

instance FromTType' Beam.BookingPayment Domain.Types.BookingPayment.BookingPayment where
  fromTType' (Beam.BookingPaymentT {..}) = do
    pure $
      Just
        Domain.Types.BookingPayment.BookingPayment
          { bookingId = Kernel.Types.Id.Id bookingId,
            id = Kernel.Types.Id.Id id,
            paymentOrderId = Kernel.Types.Id.Id paymentOrderId,
            paymentServiceType = paymentServiceType,
            status = status,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BookingPayment Domain.Types.BookingPayment.BookingPayment where
  toTType' (Domain.Types.BookingPayment.BookingPayment {..}) = do
    Beam.BookingPaymentT
      { Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.paymentOrderId = Kernel.Types.Id.getId paymentOrderId,
        Beam.paymentServiceType = paymentServiceType,
        Beam.status = status,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
