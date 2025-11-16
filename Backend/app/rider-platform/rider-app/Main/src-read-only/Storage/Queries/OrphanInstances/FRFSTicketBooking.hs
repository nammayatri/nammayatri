{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FRFSTicketBooking where

import qualified Domain.Types.FRFSTicketBooking
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FRFSTicketBooking as Beam

instance FromTType' Beam.FRFSTicketBooking Domain.Types.FRFSTicketBooking.FRFSTicketBooking where
  fromTType' (Beam.FRFSTicketBookingT {..}) = do
    pure $
      Just
        Domain.Types.FRFSTicketBooking.FRFSTicketBooking
          { _type = _type,
            bookingAuthCode = bookingAuthCode,
            bppBankAccountNumber = bppBankAccountNumber,
            bppBankCode = bppBankCode,
            bppDelayedInterest = bppDelayedInterest,
            bppItemId = bppItemId,
            bppOrderId = bppOrderId,
            bppSubscriberId = bppSubscriberId,
            bppSubscriberUrl = bppSubscriberUrl,
            cancellationCharges = cancellationCharges,
            cashbackPayoutOrderId = cashbackPayoutOrderId,
            cashbackStatus = cashbackStatus,
            childTicketQuantity = childTicketQuantity,
            customerCancelled = customerCancelled,
            discountedTickets = discountedTickets,
            estimatedPrice = Kernel.Types.Common.mkPrice currency <$> estimatedPrice,
            eventDiscountAmount = eventDiscountAmount,
            failureReason = failureReason,
            finalPrice = Kernel.Prelude.fmap (Kernel.Types.Common.mkPrice currency) finalPrice,
            fromStationCode = fromStationId,
            googleWalletJWTUrl = googleWalletJWTUrl,
            id = Kernel.Types.Id.Id id,
            integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            isBookingCancellable = isBookingCancellable,
            isFareChanged = isFareChanged,
            isSingleMode = isSingleMode,
            journeyOnInitDone = journeyOnInitDone,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            multimodalSearchRequestId = multimodalSearchRequestId,
            osBuildVersion = osBuildVersion,
            osType = osType,
            partnerOrgId = Kernel.Types.Id.Id <$> partnerOrgId,
            partnerOrgTransactionId = Kernel.Types.Id.Id <$> partnerOrgTransactionId,
            payerVpa = payerVpa,
            paymentTxnId = paymentTxnId,
            providerDescription = providerDescription,
            providerId = providerId,
            providerName = providerName,
            quantity = quantity,
            quoteId = Kernel.Types.Id.Id quoteId,
            recentLocationId = Kernel.Types.Id.Id <$> recentLocationId,
            refundAmount = refundAmount,
            riderId = Kernel.Types.Id.Id riderId,
            routeStationsJson = routeStationsJson,
            searchId = Kernel.Types.Id.Id searchId,
            startTime = startTime,
            stationsJson = stationsJson,
            status = status,
            toStationCode = toStationId,
            totalPrice = Kernel.Types.Common.mkPrice currency price,
            validTill = validTill,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSTicketBooking Domain.Types.FRFSTicketBooking.FRFSTicketBooking where
  toTType' (Domain.Types.FRFSTicketBooking.FRFSTicketBooking {..}) = do
    Beam.FRFSTicketBookingT
      { Beam._type = _type,
        Beam.bookingAuthCode = bookingAuthCode,
        Beam.bppBankAccountNumber = bppBankAccountNumber,
        Beam.bppBankCode = bppBankCode,
        Beam.bppDelayedInterest = bppDelayedInterest,
        Beam.bppItemId = bppItemId,
        Beam.bppOrderId = bppOrderId,
        Beam.bppSubscriberId = bppSubscriberId,
        Beam.bppSubscriberUrl = bppSubscriberUrl,
        Beam.cancellationCharges = cancellationCharges,
        Beam.cashbackPayoutOrderId = cashbackPayoutOrderId,
        Beam.cashbackStatus = cashbackStatus,
        Beam.childTicketQuantity = childTicketQuantity,
        Beam.customerCancelled = customerCancelled,
        Beam.discountedTickets = discountedTickets,
        Beam.estimatedPrice = Kernel.Prelude.fmap (.amount) estimatedPrice,
        Beam.eventDiscountAmount = eventDiscountAmount,
        Beam.failureReason = failureReason,
        Beam.finalPrice = Kernel.Prelude.fmap (.amount) finalPrice,
        Beam.fromStationId = fromStationCode,
        Beam.googleWalletJWTUrl = googleWalletJWTUrl,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.isBookingCancellable = isBookingCancellable,
        Beam.isFareChanged = isFareChanged,
        Beam.isSingleMode = isSingleMode,
        Beam.journeyOnInitDone = journeyOnInitDone,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.multimodalSearchRequestId = multimodalSearchRequestId,
        Beam.osBuildVersion = osBuildVersion,
        Beam.osType = osType,
        Beam.partnerOrgId = Kernel.Types.Id.getId <$> partnerOrgId,
        Beam.partnerOrgTransactionId = Kernel.Types.Id.getId <$> partnerOrgTransactionId,
        Beam.payerVpa = payerVpa,
        Beam.paymentTxnId = paymentTxnId,
        Beam.providerDescription = providerDescription,
        Beam.providerId = providerId,
        Beam.providerName = providerName,
        Beam.quantity = quantity,
        Beam.quoteId = Kernel.Types.Id.getId quoteId,
        Beam.recentLocationId = Kernel.Types.Id.getId <$> recentLocationId,
        Beam.refundAmount = refundAmount,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.routeStationsJson = routeStationsJson,
        Beam.searchId = Kernel.Types.Id.getId searchId,
        Beam.startTime = startTime,
        Beam.stationsJson = stationsJson,
        Beam.status = status,
        Beam.toStationId = toStationCode,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) totalPrice,
        Beam.price = (.amount) totalPrice,
        Beam.validTill = validTill,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
