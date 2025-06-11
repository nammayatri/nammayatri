{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FRFSSearch where

import qualified Domain.Types.FRFSSearch
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FRFSSearch as Beam
import qualified Storage.Queries.JourneyRouteDetails
import Storage.Queries.Transformers.FRFSSearch
import qualified Storage.Queries.Transformers.RouteDetails

instance FromTType' Beam.FRFSSearch Domain.Types.FRFSSearch.FRFSSearch where
  fromTType' (Beam.FRFSSearchT {..}) = do
    journeyRouteDetailsList <- Storage.Queries.JourneyRouteDetails.findAllBySearchId (Kernel.Types.Id.Id id)
    pure $
      Just
        Domain.Types.FRFSSearch.FRFSSearch
          { fromStationId = Kernel.Types.Id.Id fromStationId,
            id = Kernel.Types.Id.Id id,
            integratedBppConfigId = Kernel.Types.Id.Id <$> integratedBppConfigId,
            isOnSearchReceived = isOnSearchReceived,
            journeyLegInfo = mkJourneyLegInfo agency convenienceCost isDeleted journeyId journeyLegOrder onSearchFailed pricingId skipBooking,
            journeyLegStatus = journeyLegStatus,
            journeyRouteDetails = Storage.Queries.Transformers.RouteDetails.getTransformedJourneyRouteDetailsT journeyRouteDetailsList,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            partnerOrgId = Kernel.Types.Id.Id <$> partnerOrgId,
            partnerOrgTransactionId = Kernel.Types.Id.Id <$> partnerOrgTransactionId,
            quantity = quantity,
            recentLocationId = Kernel.Types.Id.Id <$> recentLocationId,
            riderId = Kernel.Types.Id.Id riderId,
            routeId = Kernel.Types.Id.Id <$> routeId,
            toStationId = Kernel.Types.Id.Id toStationId,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSSearch Domain.Types.FRFSSearch.FRFSSearch where
  toTType' (Domain.Types.FRFSSearch.FRFSSearch {..}) = do
    Beam.FRFSSearchT
      { Beam.fromStationId = Kernel.Types.Id.getId fromStationId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.integratedBppConfigId = Kernel.Types.Id.getId <$> integratedBppConfigId,
        Beam.isOnSearchReceived = isOnSearchReceived,
        Beam.agency = journeyLegInfo >>= (.agency),
        Beam.convenienceCost = Kernel.Prelude.fmap (.convenienceCost) journeyLegInfo,
        Beam.isDeleted = journeyLegInfo >>= (.isDeleted),
        Beam.journeyId = Kernel.Prelude.fmap (.journeyId) journeyLegInfo,
        Beam.journeyLegOrder = Kernel.Prelude.fmap (.journeyLegOrder) journeyLegInfo,
        Beam.onSearchFailed = journeyLegInfo >>= (.onSearchFailed),
        Beam.pricingId = journeyLegInfo >>= (.pricingId),
        Beam.skipBooking = Kernel.Prelude.fmap (.skipBooking) journeyLegInfo,
        Beam.journeyLegStatus = journeyLegStatus,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.partnerOrgId = Kernel.Types.Id.getId <$> partnerOrgId,
        Beam.partnerOrgTransactionId = Kernel.Types.Id.getId <$> partnerOrgTransactionId,
        Beam.quantity = quantity,
        Beam.recentLocationId = Kernel.Types.Id.getId <$> recentLocationId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.routeId = Kernel.Types.Id.getId <$> routeId,
        Beam.toStationId = Kernel.Types.Id.getId toStationId,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
