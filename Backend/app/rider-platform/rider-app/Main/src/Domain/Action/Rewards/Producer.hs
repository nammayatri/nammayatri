{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Action.Rewards.Producer
  ( RewardEvent,
    RewardEvalRequested (..),
    rewardEvalTopic,
    publishRewardEvalRequested,
  )
where

import qualified Data.Text.Encoding as TE
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC

data RewardEvent

data RewardEvalRequested = RewardEvalRequested
  { eventId :: Id RewardEvent,
    riderId :: Id DP.Person,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    completedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

rewardEvalTopic :: Text
rewardEvalTopic = "rewards.eval-requested"

publishRewardEvalRequested ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasKafkaProducer r
  ) =>
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  m ()
publishRewardEvalRequested rid moCityId completedAt = do
  mConfig <- CQRC.findByMerchantOperatingCityId moCityId
  let enabled = maybe False (.enableRewardsManagement) mConfig
  when enabled $ do
    eventId <- generateGUID
    let event =
          RewardEvalRequested
            { eventId,
              riderId = rid,
              merchantOperatingCityId = moCityId,
              completedAt
            }
    result <-
      try @_ @SomeException $
        produceMessage (rewardEvalTopic, Just (TE.encodeUtf8 rid.getId)) event
    case result of
      Right _ ->
        logInfo $
          "rewards.publish.success eventId="
            <> eventId.getId
            <> " riderId="
            <> rid.getId
      Left e ->
        logError $
          "rewards.publish.failed eventId="
            <> eventId.getId
            <> " riderId="
            <> rid.getId
            <> " moCityId="
            <> moCityId.getId
            <> " err="
            <> show e
