{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RewardOfferExtra
  ( rewardTriggerEventFromDriverCoinsEvent,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.RewardOffer as DReward
import Kernel.Prelude
import Storage.Queries.OrphanInstances.RewardOffer ()

-- | Maps driver coin event names (see `Show` / `driverCoinsEventTypeToString` in Lib.DriverCoins.Types)
-- to the reward_offer.trigger_event enum.
rewardTriggerEventFromDriverCoinsEvent :: T.Text -> Maybe DReward.RewardTriggerEvent
rewardTriggerEventFromDriverCoinsEvent = \case
  "EndRide" -> Just DReward.EndRide
  "Rating" -> Just DReward.Rating
  "Cancellation" -> Just DReward.Cancellation
  "DriverToCustomerReferral" -> Just DReward.DriverToCustomerReferral
  "CustomerToDriverReferral" -> Just DReward.CustomerToDriverReferral
  "LeaderBoard" -> Just DReward.LeaderBoard
  "Training" -> Just DReward.Training
  "BulkUploadEvent" -> Just DReward.BulkUploadEvent
  "LMS" -> Just DReward.LMS
  "LMSBonus" -> Just DReward.LMSBonus
  _ -> Nothing
