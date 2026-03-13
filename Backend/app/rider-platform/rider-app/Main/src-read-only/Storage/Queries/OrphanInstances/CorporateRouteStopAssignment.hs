{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CorporateRouteStopAssignment where

import qualified Domain.Types.CorporateRouteStopAssignment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.CorporateRouteStopAssignment as Beam

instance FromTType' Beam.CorporateRouteStopAssignment Domain.Types.CorporateRouteStopAssignment.CorporateRouteStopAssignment where
  fromTType' (Beam.CorporateRouteStopAssignmentT {..}) = do
    pure $
      Just
        Domain.Types.CorporateRouteStopAssignment.CorporateRouteStopAssignment
          { id = Kernel.Types.Id.Id id,
            routeStopId = Kernel.Types.Id.Id routeStopId,
            employeeId = Kernel.Types.Id.Id employeeId,
            effectiveFrom = effectiveFrom,
            effectiveTo = effectiveTo,
            createdAt = createdAt
          }

instance ToTType' Beam.CorporateRouteStopAssignment Domain.Types.CorporateRouteStopAssignment.CorporateRouteStopAssignment where
  toTType' (Domain.Types.CorporateRouteStopAssignment.CorporateRouteStopAssignment {..}) = do
    Beam.CorporateRouteStopAssignmentT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.routeStopId = Kernel.Types.Id.getId routeStopId,
        Beam.employeeId = Kernel.Types.Id.getId employeeId,
        Beam.effectiveFrom = effectiveFrom,
        Beam.effectiveTo = effectiveTo,
        Beam.createdAt = createdAt
      }
