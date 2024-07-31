{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FeedbackExtra where

import qualified Domain.Types.Common as DTC
import Domain.Types.Feedback
import qualified Domain.Types.Ride as Domain.Types.Ride
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Feedback as Beam
import Storage.Queries.OrphanInstances.Feedback

findFeedbackFromRatings :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Types.Id.Id Domain.Types.Ride.Ride] -> m ([Domain.Types.Feedback.Feedback]))
findFeedbackFromRatings rideIds = findAllWithKV [Se.And [Se.Is Beam.rideId $ Se.In (Kernel.Types.Id.getId <$> rideIds)]]

findOtherFeedbacks :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Types.Id.Id Domain.Types.Ride.Ride] -> Maybe Int -> m ([Domain.Types.Feedback.Feedback]))
findOtherFeedbacks rideIds limit = findAllWithOptionsKV' [Se.And [Se.Is Beam.rideId $ Se.Not $ Se.In (Kernel.Types.Id.getId <$> rideIds)]] limit Nothing
