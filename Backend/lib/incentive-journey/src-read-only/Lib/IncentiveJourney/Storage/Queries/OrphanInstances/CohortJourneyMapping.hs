{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Queries.OrphanInstances.CohortJourneyMapping where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping
import qualified Lib.IncentiveJourney.Storage.Beam.CohortJourneyMapping as Beam

instance FromTType' Beam.CohortJourneyMapping Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping where
  fromTType' (Beam.CohortJourneyMappingT {..}) = do
    pure $
      Just
        Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping
          { cohortId = Kernel.Types.Id.Id cohortId,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            journeyId = Kernel.Types.Id.Id journeyId,
            startDate = startDate,
            streakEndRewardExpirationAt = streakEndRewardExpirationAt,
            streakEndRewardType = streakEndRewardType,
            streakEndRewardValue = streakEndRewardValue,
            streakEndSubscriptionWaiveOffDaysValidFor = streakEndSubscriptionWaiveOffDaysValidFor,
            streakEndSubscriptionWaiveOffMode = streakEndSubscriptionWaiveOffMode,
            streakEndSubscriptionWaiveOffPercentage = streakEndSubscriptionWaiveOffPercentage,
            streakEndSubscriptionWaiveOffServiceName = streakEndSubscriptionWaiveOffServiceName,
            streakRange = streakRange,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CohortJourneyMapping Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping where
  toTType' (Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping {..}) = do
    Beam.CohortJourneyMappingT
      { Beam.cohortId = Kernel.Types.Id.getId cohortId,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.journeyId = Kernel.Types.Id.getId journeyId,
        Beam.startDate = startDate,
        Beam.streakEndRewardExpirationAt = streakEndRewardExpirationAt,
        Beam.streakEndRewardType = streakEndRewardType,
        Beam.streakEndRewardValue = streakEndRewardValue,
        Beam.streakEndSubscriptionWaiveOffDaysValidFor = streakEndSubscriptionWaiveOffDaysValidFor,
        Beam.streakEndSubscriptionWaiveOffMode = streakEndSubscriptionWaiveOffMode,
        Beam.streakEndSubscriptionWaiveOffPercentage = streakEndSubscriptionWaiveOffPercentage,
        Beam.streakEndSubscriptionWaiveOffServiceName = streakEndSubscriptionWaiveOffServiceName,
        Beam.streakRange = streakRange,
        Beam.updatedAt = updatedAt
      }
