{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CorporateRoute where

import qualified Domain.Types.CorporateRoute
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.CorporateRoute as Beam

instance FromTType' Beam.CorporateRoute Domain.Types.CorporateRoute.CorporateRoute where
  fromTType' (Beam.CorporateRouteT {..}) = do
    pure $
      Just
        Domain.Types.CorporateRoute.CorporateRoute
          { id = Kernel.Types.Id.Id id,
            shiftId = shiftId,
            routeCode = routeCode,
            direction = Kernel.Prelude.fromMaybe Domain.Types.CorporateRoute.PICKUP (Kernel.Prelude.readMaybe (Kernel.Prelude.toString direction)),
            estimatedDurationMinutes = estimatedDurationMinutes,
            estimatedDistanceMeters = estimatedDistanceMeters,
            vehicleTier = vehicleTier,
            maxCapacity = maxCapacity,
            status = Kernel.Prelude.fromMaybe Domain.Types.CorporateRoute.CR_ACTIVE (Kernel.Prelude.readMaybe (Kernel.Prelude.toString status)),
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CorporateRoute Domain.Types.CorporateRoute.CorporateRoute where
  toTType' (Domain.Types.CorporateRoute.CorporateRoute {..}) = do
    Beam.CorporateRouteT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.shiftId = shiftId,
        Beam.routeCode = routeCode,
        Beam.direction = Kernel.Prelude.show direction,
        Beam.estimatedDurationMinutes = estimatedDurationMinutes,
        Beam.estimatedDistanceMeters = estimatedDistanceMeters,
        Beam.vehicleTier = vehicleTier,
        Beam.maxCapacity = maxCapacity,
        Beam.status = Kernel.Prelude.show status,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
