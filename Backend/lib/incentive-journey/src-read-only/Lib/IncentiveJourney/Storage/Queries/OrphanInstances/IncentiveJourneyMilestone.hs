{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Queries.OrphanInstances.IncentiveJourneyMilestone where

import qualified Data.Text
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone
import qualified Lib.IncentiveJourney.Storage.Beam.IncentiveJourneyMilestone as Beam

instance FromTType' Beam.IncentiveJourneyMilestone Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone where
  fromTType' (Beam.IncentiveJourneyMilestoneT {..}) = do
    pure $
      Just
        Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone
          { areaType = Kernel.Prelude.maybe Nothing (Kernel.Prelude.readMaybe . Data.Text.unpack) areaType,
            conditionOperator = conditionOperator,
            conditionType = conditionType,
            conditionValue = conditionValue,
            createdAt = createdAt,
            description = description,
            id = Kernel.Types.Id.Id id,
            journeyId = Kernel.Types.Id.Id journeyId,
            name = name,
            order = order,
            rewardExpirationAt = rewardExpirationAt,
            rewardMetadata = rewardMetadata,
            rewardType = rewardType,
            rewardValue = rewardValue,
            serviceTierType = Kernel.Prelude.maybe Nothing (Kernel.Prelude.readMaybe . Data.Text.unpack) serviceTierType,
            specialLocationIds = specialLocationIds,
            timeBounds = timeBounds,
            updatedAt = updatedAt,
            vehicleCategory = Kernel.Prelude.maybe Nothing (Kernel.Prelude.readMaybe . Data.Text.unpack) vehicleCategory,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.IncentiveJourneyMilestone Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone where
  toTType' (Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone.IncentiveJourneyMilestone {..}) = do
    Beam.IncentiveJourneyMilestoneT
      { Beam.areaType = Kernel.Prelude.fmap (Data.Text.pack . Kernel.Prelude.show) areaType,
        Beam.conditionOperator = conditionOperator,
        Beam.conditionType = conditionType,
        Beam.conditionValue = conditionValue,
        Beam.createdAt = createdAt,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.journeyId = Kernel.Types.Id.getId journeyId,
        Beam.name = name,
        Beam.order = order,
        Beam.rewardExpirationAt = rewardExpirationAt,
        Beam.rewardMetadata = rewardMetadata,
        Beam.rewardType = rewardType,
        Beam.rewardValue = rewardValue,
        Beam.serviceTierType = Kernel.Prelude.fmap (Data.Text.pack . Kernel.Prelude.show) serviceTierType,
        Beam.specialLocationIds = specialLocationIds,
        Beam.timeBounds = timeBounds,
        Beam.updatedAt = updatedAt,
        Beam.vehicleCategory = Kernel.Prelude.fmap (Data.Text.pack . Kernel.Prelude.show) vehicleCategory,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
