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
import qualified Storage.Queries.JourneyRouteDetails
import qualified Storage.Queries.Transformers.RouteDetails

instance FromTType' Beam.FRFSTicketBooking Domain.Types.FRFSTicketBooking.FRFSTicketBooking where
  fromTType' (Beam.FRFSTicketBookingT {..}) = do
    journeyRouteDetailsList <- Storage.Queries.JourneyRouteDetails.findAllBySearchId (Kernel.Types.Id.Id searchId)
    pure $
      Just
        Domain.Types.FRFSTicketBooking.FRFSTicketBooking
          { _type = _type,
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
            customerCancelled = customerCancelled,
            discountedTickets = discountedTickets,
            discountsJson = discountsJson,
            estimatedPrice = Kernel.Types.Common.mkPrice currency estimatedPrice,
            eventDiscountAmount = eventDiscountAmount,
            finalPrice = Kernel.Prelude.fmap (Kernel.Types.Common.mkPrice currency) finalPrice,
            fromStationId = Kernel.Types.Id.Id fromStationId,
            googleWalletJWTUrl = googleWalletJWTUrl,
            id = Kernel.Types.Id.Id id,
            isBookingCancellable = isBookingCancellable,
            isDeleted = isDeleted,
            isFareChanged = isFareChanged,
            isSkipped = isSkipped,
            journeyId = Kernel.Types.Id.Id <$> journeyId,
            journeyLegOrder = journeyLegOrder,
            journeyLegStatus = journeyLegStatus,
            journeyOnInitDone = journeyOnInitDone,
            journeyRouteDetails = Storage.Queries.Transformers.RouteDetails.getTransformedJourneyRouteDetails journeyRouteDetailsList,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            partnerOrgId = Kernel.Types.Id.Id <$> partnerOrgId,
            partnerOrgTransactionId = Kernel.Types.Id.Id <$> partnerOrgTransactionId,
            payerVpa = payerVpa,
            paymentTxnId = paymentTxnId,
            price = Kernel.Types.Common.mkPrice currency price,
            providerDescription = providerDescription,
            providerId = providerId,
            providerName = providerName,
            quantity = quantity,
            quoteId = Kernel.Types.Id.Id quoteId,
            refundAmount = refundAmount,
            riderId = Kernel.Types.Id.Id riderId,
            routeStationsJson = routeStationsJson,
            searchId = Kernel.Types.Id.Id searchId,
            startTime = startTime,
            stationsJson = stationsJson,
            status = status,
            toStationId = Kernel.Types.Id.Id toStationId,
            validTill = validTill,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSTicketBooking Domain.Types.FRFSTicketBooking.FRFSTicketBooking where
  toTType' (Domain.Types.FRFSTicketBooking.FRFSTicketBooking {..}) = do
    Beam.FRFSTicketBookingT
      { Beam._type = _type,
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
        Beam.customerCancelled = customerCancelled,
        Beam.discountedTickets = discountedTickets,
        Beam.discountsJson = discountsJson,
        Beam.estimatedPrice = (.amount) estimatedPrice,
        Beam.eventDiscountAmount = eventDiscountAmount,
        Beam.finalPrice = Kernel.Prelude.fmap (.amount) finalPrice,
        Beam.fromStationId = Kernel.Types.Id.getId fromStationId,
        Beam.googleWalletJWTUrl = googleWalletJWTUrl,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isBookingCancellable = isBookingCancellable,
        Beam.isDeleted = isDeleted,
        Beam.isFareChanged = isFareChanged,
        Beam.isSkipped = isSkipped,
        Beam.journeyId = Kernel.Types.Id.getId <$> journeyId,
        Beam.journeyLegOrder = journeyLegOrder,
        Beam.journeyLegStatus = journeyLegStatus,
        Beam.journeyOnInitDone = journeyOnInitDone,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.partnerOrgId = Kernel.Types.Id.getId <$> partnerOrgId,
        Beam.partnerOrgTransactionId = Kernel.Types.Id.getId <$> partnerOrgTransactionId,
        Beam.payerVpa = payerVpa,
        Beam.paymentTxnId = paymentTxnId,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) price,
        Beam.price = (.amount) price,
        Beam.providerDescription = providerDescription,
        Beam.providerId = providerId,
        Beam.providerName = providerName,
        Beam.quantity = quantity,
        Beam.quoteId = Kernel.Types.Id.getId quoteId,
        Beam.refundAmount = refundAmount,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.routeStationsJson = routeStationsJson,
        Beam.searchId = Kernel.Types.Id.getId searchId,
        Beam.startTime = startTime,
        Beam.stationsJson = stationsJson,
        Beam.status = status,
        Beam.toStationId = Kernel.Types.Id.getId toStationId,
        Beam.validTill = validTill,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
