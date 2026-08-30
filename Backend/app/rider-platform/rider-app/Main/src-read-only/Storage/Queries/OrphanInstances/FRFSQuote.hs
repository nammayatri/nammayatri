{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FRFSQuote where

import qualified Domain.Types.FRFSQuote
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Storage.Beam.FRFSQuote as Beam

instance FromTType' Beam.FRFSQuote Domain.Types.FRFSQuote.FRFSQuote where
  fromTType' (Beam.FRFSQuoteT {..}) = do
    pure $
      Just
        Domain.Types.FRFSQuote.FRFSQuote
          { _type = _type,
            arrivalDate = arrivalDate,
            arrivalTime = arrivalTime,
            availableSeats = availableSeats,
            bppDelayedInterest = bppDelayedInterest,
            bppItemId = bppItemId,
            bppSubscriberId = bppSubscriberId,
            bppSubscriberUrl = bppSubscriberUrl,
            busLocationData = fromMaybe [] (Kernel.Utils.JSON.valueToMaybe =<< busLocationData),
            concessionTypeId = concessionTypeId,
            departureTime = departureTime,
            discountedTickets = discountedTickets,
            eventDiscountAmount = eventDiscountAmount,
            extraFees = extraFees,
            fareDetails = Domain.Types.FRFSQuote.FRFSFareDetails <$> appSession <*> distance <*> providerRouteId <*> ticketTypeCode <*> trainTypeCode <*> via,
            fromStationAddress = fromStationAddress,
            fromStationCode = fromStationId,
            fromStationName = fromStationName,
            fromStationPoint = Kernel.External.Maps.Types.LatLong <$> fromStationLat <*> fromStationLon,
            id = Kernel.Types.Id.Id id,
            integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            multimodalSearchRequestId = multimodalSearchRequestId,
            offerSegment = offerSegment,
            oldCacheDump = oldCacheDump,
            partnerOrgId = Kernel.Types.Id.Id <$> partnerOrgId,
            partnerOrgTransactionId = Kernel.Types.Id.Id <$> partnerOrgTransactionId,
            providerClassId = providerClassId,
            providerDescription = providerDescription,
            providerId = providerId,
            providerLayoutId = providerLayoutId,
            providerName = providerName,
            providerRefNo = providerRefNo,
            providerServiceId = providerServiceId,
            providerTripCode = providerTripCode,
            riderId = Kernel.Types.Id.Id riderId,
            routeStationsJson = routeStationsJson,
            searchId = Kernel.Types.Id.Id searchId,
            stationsJson = stationsJson,
            toStationAddress = toStationAddress,
            toStationCode = toStationId,
            toStationName = toStationName,
            toStationPoint = Kernel.External.Maps.Types.LatLong <$> toStationLat <*> toStationLon,
            tripCategory = tripCategory,
            validTill = validTill,
            vehicleNumber = vehicleNumber,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSQuote Domain.Types.FRFSQuote.FRFSQuote where
  toTType' (Domain.Types.FRFSQuote.FRFSQuote {..}) = do
    Beam.FRFSQuoteT
      { Beam._type = _type,
        Beam.arrivalDate = arrivalDate,
        Beam.arrivalTime = arrivalTime,
        Beam.availableSeats = availableSeats,
        Beam.bppDelayedInterest = bppDelayedInterest,
        Beam.bppItemId = bppItemId,
        Beam.bppSubscriberId = bppSubscriberId,
        Beam.bppSubscriberUrl = bppSubscriberUrl,
        Beam.busLocationData = Just $ toJSON busLocationData,
        Beam.concessionTypeId = concessionTypeId,
        Beam.departureTime = departureTime,
        Beam.discountedTickets = discountedTickets,
        Beam.eventDiscountAmount = eventDiscountAmount,
        Beam.extraFees = extraFees,
        Beam.appSession = fareDetails <&> (.appSession),
        Beam.distance = fareDetails <&> (.distance),
        Beam.providerRouteId = fareDetails <&> (.providerRouteId),
        Beam.ticketTypeCode = fareDetails <&> (.ticketTypeCode),
        Beam.trainTypeCode = fareDetails <&> (.trainTypeCode),
        Beam.via = fareDetails <&> (.via),
        Beam.fromStationAddress = fromStationAddress,
        Beam.fromStationId = fromStationCode,
        Beam.fromStationName = fromStationName,
        Beam.fromStationLat = (.lat) <$> fromStationPoint,
        Beam.fromStationLon = (.lon) <$> fromStationPoint,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.multimodalSearchRequestId = multimodalSearchRequestId,
        Beam.offerSegment = offerSegment,
        Beam.oldCacheDump = oldCacheDump,
        Beam.partnerOrgId = Kernel.Types.Id.getId <$> partnerOrgId,
        Beam.partnerOrgTransactionId = Kernel.Types.Id.getId <$> partnerOrgTransactionId,
        Beam.providerClassId = providerClassId,
        Beam.providerDescription = providerDescription,
        Beam.providerId = providerId,
        Beam.providerLayoutId = providerLayoutId,
        Beam.providerName = providerName,
        Beam.providerRefNo = providerRefNo,
        Beam.providerServiceId = providerServiceId,
        Beam.providerTripCode = providerTripCode,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.routeStationsJson = routeStationsJson,
        Beam.searchId = Kernel.Types.Id.getId searchId,
        Beam.stationsJson = stationsJson,
        Beam.toStationAddress = toStationAddress,
        Beam.toStationId = toStationCode,
        Beam.toStationName = toStationName,
        Beam.toStationLat = (.lat) <$> toStationPoint,
        Beam.toStationLon = (.lon) <$> toStationPoint,
        Beam.tripCategory = tripCategory,
        Beam.validTill = validTill,
        Beam.vehicleNumber = vehicleNumber,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
