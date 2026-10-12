{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Registry of the consequences currently in force for an entity, so the entity
-- can be shown what is happening to them. One Redis hash per entity; one field per
-- (consequenceType, reasonTag), so re-applying the same consequence overwrites
-- instead of piling up. This is a display record only: enforcement lives in the
-- app's own state (e.g. driver_information), which callers should reconcile against.
module Lib.BehaviorTracker.ActiveConsequences
  ( ActiveConsequence (..),
    mkActiveConsequenceKey,
    recordActive,
    readActive,
    clearActive,
  )
where

import qualified Data.Aeson as A
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.BehaviorTracker.Types

data ActiveConsequence = ActiveConsequence
  { consequenceType :: Text, -- "HARD_BLOCK", "SOFT_BLOCK", "FEATURE_BLOCK", "PERMANENT_BLOCK", "WARN", "CHARGE_FEE"
    programme :: Maybe Text, -- behaviour domain that applied it (e.g. "RATING_BEHAVIOR"); Nothing = unknown/non-engine
    reasonTag :: Maybe Text,
    appliedAt :: UTCTime,
    validTill :: Maybe UTCTime, -- Nothing = no end (permanent, or until lifted manually)
    params :: A.Value -- what a message needs: hours, tiers, feature, amount…
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | Redis hash key: "bt:active:{entityType}:{entityId}"
mkActiveConsequenceKey :: EntityType -> Text -> Text
mkActiveConsequenceKey entityType entityId = "bt:active:" <> show entityType <> ":" <> entityId

fieldOf :: ActiveConsequence -> Text
fieldOf c = c.consequenceType <> ":" <> fromMaybe "" c.reasonTag

-- Entries without an end are kept for at most a year (same horizon as permanent block keys).
noEndTtlSeconds :: Int
noEndTtlSeconds = 365 * 24 * 3600

isLive :: UTCTime -> ActiveConsequence -> Bool
isLive now c = maybe True (> now) c.validTill

-- | Record (or replace) a consequence. The hash's TTL is stretched to the latest
-- end among live entries, so no entry outlives its key.
recordActive ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  EntityType ->
  Text -> -- entityId
  ActiveConsequence ->
  m ()
recordActive entityType entityId consequence = do
  now <- getCurrentTime
  let key = mkActiveConsequenceKey entityType entityId
  existing <- readActive entityType entityId
  let others = filter ((/= fieldOf consequence) . fieldOf) existing
      ends = map (.validTill) (consequence : others)
      ttlSeconds =
        if any isNothing ends
          then noEndTtlSeconds
          else max 1 $ ceiling $ maximum (map (\e -> diffUTCTime (fromMaybe now e) now) ends)
  Redis.runInMultiCloudRedisWrite $
    Redis.withCrossAppRedis $
      Redis.hSetExp key (fieldOf consequence) consequence ttlSeconds

-- | Live consequences only; expired fields are dropped from the result and removed from Redis.
readActive ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  EntityType ->
  Text -> -- entityId
  m [ActiveConsequence]
readActive entityType entityId = do
  now <- getCurrentTime
  let key = mkActiveConsequenceKey entityType entityId
  entries :: [(Text, ActiveConsequence)] <-
    Redis.runInMultiCloudRedisForList $ Redis.withCrossAppRedis $ Redis.hGetAll key
  let live = filter (isLive now . snd) entries
      expired = filter (not . isLive now . snd) entries
  unless (null expired) $
    Redis.runInMultiCloudRedisWrite $ Redis.withCrossAppRedis $ Redis.hDel key (map fst expired)
  pure (map snd live)

-- | Remove one consequence (by type and tag), e.g. when it is lifted early.
clearActive ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  EntityType ->
  Text -> -- entityId
  Text -> -- consequenceType
  Maybe Text -> -- reasonTag
  m ()
clearActive entityType entityId consequenceType reasonTag =
  Redis.runInMultiCloudRedisWrite $
    Redis.withCrossAppRedis $
      Redis.hDel (mkActiveConsequenceKey entityType entityId) [consequenceType <> ":" <> fromMaybe "" reasonTag]
