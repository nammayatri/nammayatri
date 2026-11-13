{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Events window: an entity's most recent events for one action type, one character each,
-- oldest first, stored as one Redis string next to the bt: counters. Rules judge "offences in
-- the last X events" from it instead of calendar windows.
--
-- It mirrors the generic ELIGIBLE/ACTION counters, so any behaviour can use it (switched on
-- per behaviour by 'CounterConfig.eventsWindow', written by Lib.BehaviorTracker.Recorder):
--   * '0' = an eligible event (ELIGIBLE_COUNT +1), e.g. a ride assigned
--   * '1' = an offence (ACTION_COUNT +1), e.g. an at-fault cancel. With 'MarkLatestEligible'
--     it marks the most recent '0' (the event the action belongs to); with 'AppendAction' it
--     is appended as an event of its own.
-- Writes keep the newest Z events, Z being the caller's keep size (a city config for the
-- cancellation behaviour). Writes are Lua scripts so append/mark/trim are atomic; reads are a
-- plain GET, counted in Haskell ('countLast'), like the bt: counters are summed in Haskell.
--
-- With 'MarkLatestEligible' the window holds no event ids, so an action marks the most recent
-- '0'. When it arrives after later events (e.g. a ride that waited while others were
-- assigned), the counts stay right: the offence just leaves the window a few events later.
--
-- Example: bt:DRIVER:RIDE_CANCELLATION:EVENTS:{driverId} = "0001000101"
module Lib.BehaviorTracker.EventsWindow
  ( windowKey,
    recordEligible,
    recordAction,
    getWindow,
    countLast,
    resetWindow,
    eligibleScript,
    actionScript,
  )
where

import qualified Data.Text as T
import qualified Database.Redis as RawRedis
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common
import qualified Lib.BehaviorTracker.EventsWindowSettings as EWS
import Lib.BehaviorTracker.Types (CounterValues (..), EntityType, EventsWindowMark (..))

-- | The entity's window for one action type, hash-tagged on the entity id like the bt: counters.
windowKey :: EntityType -> Text -> Text -> Text
windowKey entityType actionType entityId = "bt:" <> show entityType <> ":" <> actionType <> ":EVENTS:{" <> entityId <> "}"

-- Both write scripts first check the stored length: a window longer than
-- 'EWS.oversizedWindowLength' was not written by these scripts (they always trim), so it is
-- started fresh instead of being worked through, and its length is returned as the last reply
-- element (0 when the window was fine) for the caller to log.

-- | KEYS: window key. ARGV: keep, TTL seconds, oversized length.
-- Appends '0'. Returns {characters stored, oversized length found or 0}.
eligibleScript :: Text
eligibleScript =
  mconcat
    [ "local keep = tonumber(ARGV[1]) ",
      "local w = redis.call('GET', KEYS[1]) or '' ",
      "local found = 0 ",
      "if #w > tonumber(ARGV[3]) then found = #w w = '' end ",
      "w = w .. '0' ",
      "if #w > keep then w = string.sub(w, -keep) end ",
      "redis.call('SET', KEYS[1], w, 'EX', ARGV[2]) ",
      "return {#w, found}"
    ]

-- | KEYS: window key. ARGV: keep, TTL seconds, oversized length, mark ("latest" or "append").
-- "latest" turns the most recent '0' into '1' (appends '1' when there is no '0'); "append"
-- appends '1'. Returns {characters stored, oversized length found or 0}.
actionScript :: Text
actionScript =
  mconcat
    [ "local keep = tonumber(ARGV[1]) ",
      "local w = redis.call('GET', KEYS[1]) or '' ",
      "local found = 0 ",
      "if #w > tonumber(ARGV[3]) then found = #w w = '' end ",
      "local pos = nil ",
      "if ARGV[4] == 'latest' then pos = string.find(w, '0[^0]*$') end ",
      "if pos then w = string.sub(w, 1, pos - 1) .. '1' .. string.sub(w, pos + 1) else w = w .. '1' end ",
      "if #w > keep then w = string.sub(w, -keep) end ",
      "redis.call('SET', KEYS[1], w, 'EX', ARGV[2]) ",
      "return {#w, found}"
    ]

ttlSeconds :: Int
ttlSeconds = EWS.windowTtlDays * 24 * 60 * 60

numArg :: Int -> Text
numArg = show

-- | The keep size actually used: at least 1, at most 'EWS.maxWindowSize'.
clampKeep :: Int -> Int
clampKeep keep = max 1 (min keep EWS.maxWindowSize)

-- | Runs a window script on the entity's key, on both Redis clusters like the bt:
-- counters; the reply comes from the primary.
runWindowScript ::
  (Redis.HedisFlow m r, TryException m, RawRedis.RedisResult a) =>
  Text ->
  Text ->
  [Text] ->
  m a
runWindowScript script key args =
  Redis.runInMultiCloudRedisWrite $
    Redis.withCrossAppRedis $ do
      wKey <- Redis.buildKey key
      Redis.runHedis (RawRedis.eval (encodeUtf8 script) [wKey] (map encodeUtf8 args))

-- | An eligible event (where the counters add ELIGIBLE +1): append '0', keeping the newest
-- @keep@ events. Returns the length of an oversized window that was cleared first (see
-- 'EWS.oversizedWindowLength'), for the caller to log.
recordEligible ::
  (Redis.HedisFlow m r, TryException m) =>
  EntityType ->
  -- | action type, e.g. "RIDE_CANCELLATION"
  Text ->
  -- | entity id, e.g. the driver id
  Text ->
  -- | events to keep (Z)
  Int ->
  m (Maybe Int)
recordEligible entityType actionType entityId keep =
  writeReply key =<< runWindowScript eligibleScript key [numArg (clampKeep keep), numArg ttlSeconds, numArg EWS.oversizedWindowLength]
  where
    key = windowKey entityType actionType entityId

-- | An offence (where the counters add ACTION +1): mark a '1' as the behaviour's
-- 'EventsWindowMark' says, keeping the newest @keep@ events. Returns the length of an
-- oversized window that was cleared first.
recordAction ::
  (Redis.HedisFlow m r, TryException m) =>
  EntityType ->
  -- | action type, e.g. "RIDE_CANCELLATION"
  Text ->
  -- | entity id, e.g. the driver id
  Text ->
  EventsWindowMark ->
  -- | events to keep (Z)
  Int ->
  m (Maybe Int)
recordAction entityType actionType entityId mark keep =
  writeReply key =<< runWindowScript actionScript key [numArg (clampKeep keep), numArg ttlSeconds, numArg EWS.oversizedWindowLength, markArg]
  where
    key = windowKey entityType actionType entityId
    markArg = case mark of
      MarkLatestEligible -> "latest"
      AppendAction -> "append"

writeReply :: Redis.HedisFlow m r => Text -> [Integer] -> m (Maybe Int)
writeReply key = \case
  [_, found] -> pure $ oversizedFound found
  reply -> throwError $ InternalError $ "Events window script returned an unexpected reply for " <> key <> ": " <> show reply

-- | The scripts reply 0 when the window was fine, else the oversized length they found.
oversizedFound :: Integer -> Maybe Int
oversizedFound found = if found > 0 then Just (fromInteger found) else Nothing

-- | The entity's window as stored ("" when there is none), read from the primary cluster with
-- one plain GET. Count it with 'countLast'.
getWindow ::
  (Redis.HedisFlow m r, TryException m) =>
  EntityType ->
  -- | action type, e.g. "RIDE_CANCELLATION"
  Text ->
  -- | entity id, e.g. the driver id
  Text ->
  m Text
getWindow entityType actionType entityId = do
  mbWindow <-
    Redis.withCrossAppRedis $ do
      wKey <- Redis.buildKey (windowKey entityType actionType entityId)
      Redis.runHedis (RawRedis.get wKey)
  pure $ maybe "" decodeUtf8 mbWindow

-- | Counts of the newest @x@ events of a window: actionCount = offences ('1'), eligibleCount =
-- events counted (fewer than x when fewer are stored), rate = offence %. A window longer than
-- 'EWS.oversizedWindowLength' was not written by the window scripts, so it counts as empty and
-- its length is returned for the caller to log (the next write clears it).
countLast :: Int -> Text -> (CounterValues, Maybe Int)
countLast x window
  | T.length window > EWS.oversizedWindowLength = (counts 0 0, Just (T.length window))
  | otherwise =
    let tailX = T.takeEnd (max 1 (min x EWS.maxWindowSize)) window
     in (counts (T.count "1" tailX) (T.length tailX), Nothing)
  where
    counts :: Int -> Int -> CounterValues
    counts offences events =
      let o = toInteger offences
          e = toInteger events
       in CounterValues {actionCount = o, eligibleCount = e, rate = (o * 100) `div` max 1 e}

-- | Clears the entity's window, e.g. after a rule's hard block.
resetWindow :: (Redis.HedisFlow m r, TryException m) => EntityType -> Text -> Text -> m ()
resetWindow entityType actionType entityId =
  Redis.runInMultiCloudRedisWrite $
    Redis.withCrossAppRedis $
      Redis.del (windowKey entityType actionType entityId)
