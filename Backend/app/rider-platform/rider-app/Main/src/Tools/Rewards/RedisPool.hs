{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}

module Tools.Rewards.RedisPool
  ( poolKey,
    inflightKey,
    claimFromPool,
    removeFromInflight,
    pushBackToPool,
    staleInflightCodes,
    poolSize,
    bulkSeedPool,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.RewardCampaign as DRCmp
import qualified Domain.Types.RewardCohort as DRC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common

poolKey :: Id DRCmp.RewardCampaign -> Id DRC.RewardCohort -> Text
poolKey (Id c) (Id co) = "reward:pool:" <> c <> ":" <> co

-- | Inflight is a sorted set keyed by code, scored by epoch-second claim time.
inflightKey :: Id DRCmp.RewardCampaign -> Id DRC.RewardCohort -> Text
inflightKey (Id c) (Id co) = "reward:inflight:" <> c <> ":" <> co

-- | Claim one code from the pool into the inflight sorted set.
-- Uses LPUSH seed + RPOP claim (FIFO; Kernel.Storage.Hedis has no lPop).
claimFromPool ::
  (Hedis.HedisFlow m env, MonadTime m) =>
  Id DRCmp.RewardCampaign ->
  Id DRC.RewardCohort ->
  m (Maybe Text)
claimFromPool cId coId = Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ do
  mbCode <- Hedis.rPop (poolKey cId coId)
  case mbCode of
    Nothing -> pure Nothing
    Just code -> do
      now <- getCurrentTime
      let score = realToFrac (utcTimeToPOSIXSeconds now) :: Double
      void $ Hedis.zAdd (inflightKey cId coId) [(score, code)]
      pure (Just code)

removeFromInflight ::
  (Hedis.HedisFlow m env) =>
  Id DRCmp.RewardCampaign ->
  Id DRC.RewardCohort ->
  Text ->
  m ()
removeFromInflight cId coId code =
  Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ void $ Hedis.zRem (inflightKey cId coId) [code]

pushBackToPool ::
  (Hedis.HedisFlow m env) =>
  Id DRCmp.RewardCampaign ->
  Id DRC.RewardCohort ->
  Text ->
  m ()
pushBackToPool cId coId code =
  Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ void $ Hedis.lPush (poolKey cId coId) (NE.singleton code)

staleInflightCodes ::
  (Hedis.HedisFlow m env, MonadTime m) =>
  Id DRCmp.RewardCampaign ->
  Id DRC.RewardCohort ->
  Int ->
  m [Text]
staleInflightCodes cId coId maxAgeSeconds = Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ do
  now <- getCurrentTime
  let cutoff = realToFrac (utcTimeToPOSIXSeconds now) - fromIntegral maxAgeSeconds :: Double
  codes <- Hedis.zRangeByScore (inflightKey cId coId) 0 cutoff
  pure $ map TE.decodeUtf8 codes

poolSize ::
  (Hedis.HedisFlow m env) =>
  Id DRCmp.RewardCampaign ->
  Id DRC.RewardCohort ->
  m Integer
poolSize cId coId =
  Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.lLen (poolKey cId coId)

bulkSeedPool ::
  (Hedis.HedisFlow m env) =>
  Id DRCmp.RewardCampaign ->
  Id DRC.RewardCohort ->
  [Text] ->
  m ()
bulkSeedPool cId coId codes =
  Hedis.runInMasterCloudRedisCellWithCrossAppRedis $
    case NE.nonEmpty codes of
      Nothing -> pure ()
      Just ne -> void $ Hedis.lPush (poolKey cId coId) ne
