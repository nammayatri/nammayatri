{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FRFSSearch where

import qualified Domain.Types.FRFSSearch
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Storage.Beam.FRFSSearch as Beam

instance FromTType' Beam.FRFSSearch Domain.Types.FRFSSearch.FRFSSearch where
  fromTType' (Beam.FRFSSearchT {..}) = do
    pure $
      Just
        Domain.Types.FRFSSearch.FRFSSearch
          { busLocationData = fromMaybe [] (Kernel.Utils.JSON.valueToMaybe =<< busLocationData),
            fromStationAddress = fromStationAddress,
            fromStationCode = fromStationId,
            fromStationName = fromStationName,
            fromStationPoint = Kernel.External.Maps.Types.LatLong <$> fromStationLat <*> fromStationLon,
            id = Kernel.Types.Id.Id id,
            integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            isOnSearchReceived = isOnSearchReceived,
            isSingleMode = isSingleMode,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            multimodalSearchRequestId = multimodalSearchRequestId,
            onSearchFailed = onSearchFailed,
            partnerOrgId = Kernel.Types.Id.Id <$> partnerOrgId,
            partnerOrgTransactionId = Kernel.Types.Id.Id <$> partnerOrgTransactionId,
            quantity = quantity,
            recentLocationId = Kernel.Types.Id.Id <$> recentLocationId,
            riderId = Kernel.Types.Id.Id riderId,
            routeCode = routeId,
            searchAsParentStops = searchAsParentStops,
            toStationAddress = toStationAddress,
            toStationCode = toStationId,
            toStationName = toStationName,
            toStationPoint = Kernel.External.Maps.Types.LatLong <$> toStationLat <*> toStationLon,
            validTill = validTill,
            vehicleNumber = vehicleNumber,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSSearch Domain.Types.FRFSSearch.FRFSSearch where
  toTType' (Domain.Types.FRFSSearch.FRFSSearch {..}) = do
    Beam.FRFSSearchT
      { Beam.busLocationData = Just $ toJSON busLocationData,
        Beam.fromStationAddress = fromStationAddress,
        Beam.fromStationId = fromStationCode,
        Beam.fromStationName = fromStationName,
        Beam.fromStationLat = (.lat) <$> fromStationPoint,
        Beam.fromStationLon = (.lon) <$> fromStationPoint,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.isOnSearchReceived = isOnSearchReceived,
        Beam.isSingleMode = isSingleMode,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.multimodalSearchRequestId = multimodalSearchRequestId,
        Beam.onSearchFailed = onSearchFailed,
        Beam.partnerOrgId = Kernel.Types.Id.getId <$> partnerOrgId,
        Beam.partnerOrgTransactionId = Kernel.Types.Id.getId <$> partnerOrgTransactionId,
        Beam.quantity = quantity,
        Beam.recentLocationId = Kernel.Types.Id.getId <$> recentLocationId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.routeId = routeCode,
        Beam.searchAsParentStops = searchAsParentStops,
        Beam.toStationAddress = toStationAddress,
        Beam.toStationId = toStationCode,
        Beam.toStationName = toStationName,
        Beam.toStationLat = (.lat) <$> toStationPoint,
        Beam.toStationLon = (.lon) <$> toStationPoint,
        Beam.validTill = validTill,
        Beam.vehicleNumber = vehicleNumber,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
