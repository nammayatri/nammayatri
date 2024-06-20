{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PersonStats where

import qualified Domain.Types.PersonStats
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PersonStats as Beam

instance FromTType' Beam.PersonStats Domain.Types.PersonStats.PersonStats where
  fromTType' (Beam.PersonStatsT {..}) = do
    pure $
      Just
        Domain.Types.PersonStats.PersonStats
          { personId = Kernel.Types.Id.Id personId,
            userCancelledRides = userCancelledRides,
            driverCancelledRides = driverCancelledRides,
            completedRides = completedRides,
            weekendRides = weekendRides,
            weekdayRides = weekdayRides,
            offPeakRides = offPeakRides,
            eveningPeakRides = eveningPeakRides,
            morningPeakRides = morningPeakRides,
            weekendPeakRides = weekendPeakRides,
            referralCount = referralCount,
            createdAt = Kernel.Prelude.fromMaybe updatedAt createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PersonStats Domain.Types.PersonStats.PersonStats where
  toTType' (Domain.Types.PersonStats.PersonStats {..}) = do
    Beam.PersonStatsT
      { Beam.personId = Kernel.Types.Id.getId personId,
        Beam.userCancelledRides = userCancelledRides,
        Beam.driverCancelledRides = driverCancelledRides,
        Beam.completedRides = completedRides,
        Beam.weekendRides = weekendRides,
        Beam.weekdayRides = weekdayRides,
        Beam.offPeakRides = offPeakRides,
        Beam.eveningPeakRides = eveningPeakRides,
        Beam.morningPeakRides = morningPeakRides,
        Beam.weekendPeakRides = weekendPeakRides,
        Beam.referralCount = referralCount,
        Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.updatedAt = updatedAt
      }
