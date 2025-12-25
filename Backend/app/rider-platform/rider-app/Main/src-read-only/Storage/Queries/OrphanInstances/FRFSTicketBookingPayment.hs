{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FRFSTicketBookingPayment where

import qualified Domain.Types.FRFSTicketBookingPayment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FRFSTicketBookingPayment as Beam

instance FromTType' Beam.FRFSTicketBookingPayment Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment where
  fromTType' (Beam.FRFSTicketBookingPaymentT {..}) = do
    pure $
      Just
        Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment
          { frfsQuoteId = Kernel.Types.Id.Id <$> frfsQuoteId,
            frfsTicketBookingId = Kernel.Types.Id.Id frfsTicketBookingId,
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
      { Beam.frfsQuoteId = Kernel.Types.Id.getId <$> frfsQuoteId,
        Beam.frfsTicketBookingId = Kernel.Types.Id.getId frfsTicketBookingId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.paymentOrderId = Kernel.Types.Id.getId paymentOrderId,
        Beam.status = status,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
