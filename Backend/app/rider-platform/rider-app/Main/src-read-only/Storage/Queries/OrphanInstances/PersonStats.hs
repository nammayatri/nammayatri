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
          { backlogPayoutAmount = Kernel.Prelude.fromMaybe 0 backlogPayoutAmount,
            backlogPayoutStatus = backlogPayoutStatus,
            completedRides = completedRides,
            createdAt = Kernel.Prelude.fromMaybe updatedAt createdAt,
            driverCancelledRides = driverCancelledRides,
            eveningPeakRides = eveningPeakRides,
            isBackfilled = isBackfilled,
            morningPeakRides = morningPeakRides,
            offPeakRides = offPeakRides,
            personId = Kernel.Types.Id.Id personId,
            referralAmountPaid = Kernel.Prelude.fromMaybe 0 referralAmountPaid,
            referralCount = referralCount,
            referralEarnings = Kernel.Prelude.fromMaybe 0 referralEarnings,
            referredByEarnings = Kernel.Prelude.fromMaybe 0 referredByEarnings,
            referredByEarningsPayoutStatus = referredByEarningsPayoutStatus,
            ticketsBookedInEvent = ticketsBookedInEvent,
            updatedAt = updatedAt,
            userCancelledRides = userCancelledRides,
            validActivations = Kernel.Prelude.fromMaybe 0 validActivations,
            weekdayRides = weekdayRides,
            weekendPeakRides = weekendPeakRides,
            weekendRides = weekendRides
          }

instance ToTType' Beam.PersonStats Domain.Types.PersonStats.PersonStats where
  toTType' (Domain.Types.PersonStats.PersonStats {..}) = do
    Beam.PersonStatsT
      { Beam.backlogPayoutAmount = Kernel.Prelude.Just backlogPayoutAmount,
        Beam.backlogPayoutStatus = backlogPayoutStatus,
        Beam.completedRides = completedRides,
        Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.driverCancelledRides = driverCancelledRides,
        Beam.eveningPeakRides = eveningPeakRides,
        Beam.isBackfilled = isBackfilled,
        Beam.morningPeakRides = morningPeakRides,
        Beam.offPeakRides = offPeakRides,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.referralAmountPaid = Kernel.Prelude.Just referralAmountPaid,
        Beam.referralCount = referralCount,
        Beam.referralEarnings = Kernel.Prelude.Just referralEarnings,
        Beam.referredByEarnings = Kernel.Prelude.Just referredByEarnings,
        Beam.referredByEarningsPayoutStatus = referredByEarningsPayoutStatus,
        Beam.ticketsBookedInEvent = ticketsBookedInEvent,
        Beam.updatedAt = updatedAt,
        Beam.userCancelledRides = userCancelledRides,
        Beam.validActivations = Kernel.Prelude.Just validActivations,
        Beam.weekdayRides = weekdayRides,
        Beam.weekendPeakRides = weekendPeakRides,
        Beam.weekendRides = weekendRides
      }
