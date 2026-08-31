{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Queries.OrphanInstances.IncentiveJourneyStats where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats
import qualified Lib.IncentiveJourney.Storage.Beam.IncentiveJourneyStats as Beam

instance FromTType' Beam.IncentiveJourneyStats Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats where
  fromTType' (Beam.IncentiveJourneyStatsT {..}) = do
    pure $
      Just
        Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats
          { cohortId = Kernel.Types.Id.Id cohortId,
            conditionOperator = conditionOperator,
            conditionType = conditionType,
            conditionValue = conditionValue,
            createdAt = createdAt,
            currentValue = currentValue,
            id = Kernel.Types.Id.Id id,
            journeyId = Kernel.Types.Id.Id journeyId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            milestoneId = Kernel.Types.Id.Id milestoneId,
            periodKey = periodKey,
            personId = Kernel.Types.Id.Id personId,
            rewardType = rewardType,
            rewardValue = rewardValue,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.IncentiveJourneyStats Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats where
  toTType' (Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats.IncentiveJourneyStats {..}) = do
    Beam.IncentiveJourneyStatsT
      { Beam.cohortId = Kernel.Types.Id.getId cohortId,
        Beam.conditionOperator = conditionOperator,
        Beam.conditionType = conditionType,
        Beam.conditionValue = conditionValue,
        Beam.createdAt = createdAt,
        Beam.currentValue = currentValue,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.journeyId = Kernel.Types.Id.getId journeyId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.milestoneId = Kernel.Types.Id.getId milestoneId,
        Beam.periodKey = periodKey,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.rewardType = rewardType,
        Beam.rewardValue = rewardValue,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
