{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FRFSSearch where

import qualified Domain.Types.FRFSSearch
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FRFSSearch as Beam

instance FromTType' Beam.FRFSSearch Domain.Types.FRFSSearch.FRFSSearch where
  fromTType' (Beam.FRFSSearchT {..}) = do
    pure $
      Just
        Domain.Types.FRFSSearch.FRFSSearch
          { fromStationCode = fromStationId,
            id = Kernel.Types.Id.Id id,
            integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            isOnSearchReceived = isOnSearchReceived,
            multimodalSearchRequestId = multimodalSearchRequestId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            onSearchFailed = onSearchFailed,
            partnerOrgId = Kernel.Types.Id.Id <$> partnerOrgId,
            partnerOrgTransactionId = Kernel.Types.Id.Id <$> partnerOrgTransactionId,
            quantity = quantity,
            recentLocationId = Kernel.Types.Id.Id <$> recentLocationId,
            riderId = Kernel.Types.Id.Id riderId,
            routeCode = routeId,
            toStationCode = toStationId,
            validTill = validTill,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSSearch Domain.Types.FRFSSearch.FRFSSearch where
  toTType' (Domain.Types.FRFSSearch.FRFSSearch {..}) = do
    Beam.FRFSSearchT
      { Beam.fromStationId = fromStationCode,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.isOnSearchReceived = isOnSearchReceived,
        Beam.multimodalSearchRequestId = multimodalSearchRequestId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.onSearchFailed = onSearchFailed,
        Beam.partnerOrgId = Kernel.Types.Id.getId <$> partnerOrgId,
        Beam.partnerOrgTransactionId = Kernel.Types.Id.getId <$> partnerOrgTransactionId,
        Beam.quantity = quantity,
        Beam.recentLocationId = Kernel.Types.Id.getId <$> recentLocationId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.routeId = routeCode,
        Beam.toStationId = toStationCode,
        Beam.validTill = validTill,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
