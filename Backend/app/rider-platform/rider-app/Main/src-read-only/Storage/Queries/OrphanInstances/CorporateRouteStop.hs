{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CorporateRouteStop where

import qualified Domain.Types.CorporateRouteStop
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.CorporateRouteStop as Beam

instance FromTType' Beam.CorporateRouteStop Domain.Types.CorporateRouteStop.CorporateRouteStop where
  fromTType' (Beam.CorporateRouteStopT {..}) = do
    pure $
      Just
        Domain.Types.CorporateRouteStop.CorporateRouteStop
          { id = Kernel.Types.Id.Id id,
            routeId = Kernel.Types.Id.Id routeId,
            sequence = sequence,
            lat = lat,
            lon = lon,
            address = address,
            estimatedArrivalOffset = estimatedArrivalOffset,
            createdAt = createdAt
          }

instance ToTType' Beam.CorporateRouteStop Domain.Types.CorporateRouteStop.CorporateRouteStop where
  toTType' (Domain.Types.CorporateRouteStop.CorporateRouteStop {..}) = do
    Beam.CorporateRouteStopT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.routeId = Kernel.Types.Id.getId routeId,
        Beam.sequence = sequence,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.address = address,
        Beam.estimatedArrivalOffset = estimatedArrivalOffset,
        Beam.createdAt = createdAt
      }
