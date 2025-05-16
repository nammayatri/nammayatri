{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FRFSQuote where

import qualified Domain.Types.FRFSQuote
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FRFSQuote as Beam

instance FromTType' Beam.FRFSQuote Domain.Types.FRFSQuote.FRFSQuote where
  fromTType' (Beam.FRFSQuoteT {..}) = do
    pure $
      Just
        Domain.Types.FRFSQuote.FRFSQuote
          { _type = _type,
            bppDelayedInterest = bppDelayedInterest,
            bppItemId = bppItemId,
            bppSubscriberId = bppSubscriberId,
            bppSubscriberUrl = bppSubscriberUrl,
            childPrice = Kernel.Types.Common.mkPrice currency <$> childPrice,
            discountedTickets = discountedTickets,
            discountsJson = discountsJson,
            eventDiscountAmount = eventDiscountAmount,
            fareDetails = Domain.Types.FRFSQuote.FRFSFareDetails <$> appSession <*> distance <*> providerRouteId <*> sdkToken <*> ticketTypeCode <*> trainTypeCode <*> via,
            fromStationId = Kernel.Types.Id.Id fromStationId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            oldCacheDump = oldCacheDump,
            partnerOrgId = Kernel.Types.Id.Id <$> partnerOrgId,
            partnerOrgTransactionId = Kernel.Types.Id.Id <$> partnerOrgTransactionId,
            price = Kernel.Types.Common.mkPrice currency price,
            providerDescription = providerDescription,
            providerId = providerId,
            providerName = providerName,
            quantity = quantity,
            riderId = Kernel.Types.Id.Id riderId,
            routeStationsJson = routeStationsJson,
            searchId = Kernel.Types.Id.Id searchId,
            stationsJson = stationsJson,
            toStationId = Kernel.Types.Id.Id toStationId,
            validTill = validTill,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSQuote Domain.Types.FRFSQuote.FRFSQuote where
  toTType' (Domain.Types.FRFSQuote.FRFSQuote {..}) = do
    Beam.FRFSQuoteT
      { Beam._type = _type,
        Beam.bppDelayedInterest = bppDelayedInterest,
        Beam.bppItemId = bppItemId,
        Beam.bppSubscriberId = bppSubscriberId,
        Beam.bppSubscriberUrl = bppSubscriberUrl,
        Beam.childPrice = Kernel.Prelude.fmap (.amount) childPrice,
        Beam.discountedTickets = discountedTickets,
        Beam.discountsJson = discountsJson,
        Beam.eventDiscountAmount = eventDiscountAmount,
        Beam.appSession = fareDetails <&> (.appSession),
        Beam.distance = fareDetails <&> (.distance),
        Beam.providerRouteId = fareDetails <&> (.providerRouteId),
        Beam.sdkToken = fareDetails <&> (.sdkToken),
        Beam.ticketTypeCode = fareDetails <&> (.ticketTypeCode),
        Beam.trainTypeCode = fareDetails <&> (.trainTypeCode),
        Beam.via = fareDetails <&> (.via),
        Beam.fromStationId = Kernel.Types.Id.getId fromStationId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.oldCacheDump = oldCacheDump,
        Beam.partnerOrgId = Kernel.Types.Id.getId <$> partnerOrgId,
        Beam.partnerOrgTransactionId = Kernel.Types.Id.getId <$> partnerOrgTransactionId,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) price,
        Beam.price = (.amount) price,
        Beam.providerDescription = providerDescription,
        Beam.providerId = providerId,
        Beam.providerName = providerName,
        Beam.quantity = quantity,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.routeStationsJson = routeStationsJson,
        Beam.searchId = Kernel.Types.Id.getId searchId,
        Beam.stationsJson = stationsJson,
        Beam.toStationId = Kernel.Types.Id.getId toStationId,
        Beam.validTill = validTill,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
