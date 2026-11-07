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
-- Every window keeps the newest 'EWS.maxWindowSize' events, whatever X the live rulebook
-- uses, so a larger X later already has the history.
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
    readWindowCounts,
    resetWindow,
    eligibleScript,
    actionScript,
    readScript,
  )
where

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

-- Every script first checks the stored length: a window longer than
-- 'EWS.oversizedWindowLength' was not written by these scripts (they always trim), so it is
-- treated as empty instead of being worked through, and its length is returned as the last
-- reply element (0 when the window was fine) for the caller to log.

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

-- | KEYS: window key. ARGV: read (X), oversized length. Writes nothing; returns
-- {offences in the last X, events in the last X, oversized length found or 0}. An
-- oversized window counts as empty here; the next write clears it.
readScript :: Text
readScript =
  mconcat
    [ "local w = redis.call('GET', KEYS[1]) or '' ",
      "if #w > tonumber(ARGV[2]) then return {0, 0, #w} end ",
      "local tail = string.sub(w, -tonumber(ARGV[1])) ",
      "local _, off = string.gsub(tail, '1', '') ",
      "return {off, #tail, 0}"
    ]

ttlSeconds :: Int
ttlSeconds = EWS.windowTtlDays * 24 * 60 * 60

numArg :: Int -> Text
numArg = show

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

-- | An eligible event (where the counters add ELIGIBLE +1): append '0'. Returns the length of
-- an oversized window that was cleared first (see 'EWS.oversizedWindowLength'), for the caller to log.
recordEligible ::
  (Redis.HedisFlow m r, TryException m) =>
  EntityType ->
  -- | action type, e.g. "RIDE_CANCELLATION"
  Text ->
  -- | entity id, e.g. the driver id
  Text ->
  m (Maybe Int)
recordEligible entityType actionType entityId =
  writeReply key =<< runWindowScript eligibleScript key [numArg EWS.maxWindowSize, numArg ttlSeconds, numArg EWS.oversizedWindowLength]
  where
    key = windowKey entityType actionType entityId

-- | An offence (where the counters add ACTION +1): mark a '1' as the behaviour's
-- 'EventsWindowMark' says. Returns the length of an oversized window that was cleared first.
recordAction ::
  (Redis.HedisFlow m r, TryException m) =>
  EntityType ->
  -- | action type, e.g. "RIDE_CANCELLATION"
  Text ->
  -- | entity id, e.g. the driver id
  Text ->
  EventsWindowMark ->
  m (Maybe Int)
recordAction entityType actionType entityId mark =
  writeReply key =<< runWindowScript actionScript key [numArg EWS.maxWindowSize, numArg ttlSeconds, numArg EWS.oversizedWindowLength, markArg]
  where
    key = windowKey entityType actionType entityId
    markArg = case mark of
      MarkLatestEligible -> "latest"
      AppendAction -> "append"

writeReply :: Redis.HedisFlow m r => Text -> [Integer] -> m (Maybe Int)
writeReply key = \case
  [_, found] -> pure $ oversizedFound found
  reply -> throwError $ InternalError $ "Events window script returned an unexpected reply for " <> key <> ": " <> show reply

-- | Counts of the last @readSize@ events: actionCount = offences ('1'), eligibleCount = events,
-- rate = offence %. Reads the primary cluster only and writes nothing. An oversized window
-- counts as empty and its length is returned for the caller to log.
readWindowCounts ::
  (Redis.HedisFlow m r, TryException m) =>
  EntityType ->
  -- | action type, e.g. "RIDE_CANCELLATION"
  Text ->
  -- | entity id, e.g. the driver id
  Text ->
  -- | X: how many of the newest events to count
  Int ->
  m (CounterValues, Maybe Int)
readWindowCounts entityType actionType entityId readSize = do
  let key = windowKey entityType actionType entityId
      readN = max 1 (min readSize EWS.maxWindowSize)
  reply :: [Integer] <-
    Redis.withCrossAppRedis $ do
      wKey <- Redis.buildKey key
      Redis.runHedis (RawRedis.eval (encodeUtf8 readScript) [wKey] (map encodeUtf8 [numArg readN, numArg EWS.oversizedWindowLength]))
  case reply of
    [offences, events, found] ->
      pure (CounterValues {actionCount = offences, eligibleCount = events, rate = (offences * 100) `div` max 1 events}, oversizedFound found)
    _ -> throwError $ InternalError $ "Events window script returned an unexpected reply for " <> key <> ": " <> show reply

-- | The scripts reply 0 when the window was fine, else the oversized length they found.
oversizedFound :: Integer -> Maybe Int
oversizedFound found = if found > 0 then Just (fromInteger found) else Nothing

-- | Clears the entity's window, e.g. after a rule's hard block.
resetWindow :: (Redis.HedisFlow m r, TryException m) => EntityType -> Text -> Text -> m ()
resetWindow entityType actionType entityId =
  Redis.runInMultiCloudRedisWrite $
    Redis.withCrossAppRedis $
      Redis.del (windowKey entityType actionType entityId)
