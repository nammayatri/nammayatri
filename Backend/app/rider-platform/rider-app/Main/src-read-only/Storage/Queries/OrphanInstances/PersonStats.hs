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
          { backfilledFromCkhTill = backfilledFromCkhTill,
            completedRides = completedRides,
            createdAt = Kernel.Prelude.fromMaybe updatedAt createdAt,
            driverCancelledRides = driverCancelledRides,
            eveningPeakRides = eveningPeakRides,
            isBackfilled = isBackfilled,
            morningPeakRides = morningPeakRides,
            offPeakRides = offPeakRides,
            personId = Kernel.Types.Id.Id personId,
            referralCount = referralCount,
            ticketsBookedInEvent = ticketsBookedInEvent,
            updatedAt = updatedAt,
            userCancelledRides = userCancelledRides,
            weekdayRides = weekdayRides,
            weekendPeakRides = weekendPeakRides,
            weekendRides = weekendRides
          }

instance ToTType' Beam.PersonStats Domain.Types.PersonStats.PersonStats where
  toTType' (Domain.Types.PersonStats.PersonStats {..}) = do
    Beam.PersonStatsT
      { Beam.backfilledFromCkhTill = backfilledFromCkhTill,
        Beam.completedRides = completedRides,
        Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.driverCancelledRides = driverCancelledRides,
        Beam.eveningPeakRides = eveningPeakRides,
        Beam.isBackfilled = isBackfilled,
        Beam.morningPeakRides = morningPeakRides,
        Beam.offPeakRides = offPeakRides,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.referralCount = referralCount,
        Beam.ticketsBookedInEvent = ticketsBookedInEvent,
        Beam.updatedAt = updatedAt,
        Beam.userCancelledRides = userCancelledRides,
        Beam.weekdayRides = weekdayRides,
        Beam.weekendPeakRides = weekendPeakRides,
        Beam.weekendRides = weekendRides
      }
