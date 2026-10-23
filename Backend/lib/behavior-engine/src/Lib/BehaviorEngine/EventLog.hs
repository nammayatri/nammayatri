{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Behaviour metrics: one Kafka event per nudge / warn / block / communication a
-- behaviour rulebook decides on. ClickHouse reads the topic into
-- atlas_kafka.behaviour_consequence_events (see dev/clickhouse/sql-seed/atlas-kafka-seed.sql),
-- which backs the Behaviour Programmes metrics in the control center.
--
-- Emission is fire-and-forget: it runs in a fork and never fails the caller's flow.
-- Ops kill switch: set the Redis key to 1 (picked up within a minute), delete it to resume.
-- Each app adds its own key prefix, so set both:
--   dynamic-offer-driver-app:BehaviourEvents:Disabled  (driver app)
--   BehaviourEvents:Disabled                           (kafka-consumers, e.g. GPS toll)
module Lib.BehaviorEngine.EventLog
  ( BehaviourEventSource (..),
    BehaviourEventContext (..),
    BehaviourEventCity (..),
    BehaviourEventDetail (..),
    emitBehaviourEvents,
    snapshotEventContext,
    outputEventDetails,
    consequenceEventDetail,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Data.Time (UTCTime (..), diffTimeToPicoseconds)
import qualified Data.Time.Format as Time
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.BehaviorEngine.Types (OrchestratedOutput (..))
import Lib.BehaviorTracker.Types (BehaviorSnapshot (..))
import qualified Lib.CommunicationEngine.Types as CMT
import qualified Lib.ConsequenceEngine.Parser as CEParser
import qualified Lib.ConsequenceEngine.Types as CET
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT

behaviourEventsTopic :: Text
behaviourEventsTopic = "behaviour-consequence-events"

behaviourEventsDisabledKey :: Text
behaviourEventsDisabledKey = "BehaviourEvents:Disabled"

-- | RULEBOOK: decided by a behaviour rulebook. LEGACY: a hard-coded path outside the rulebook.
data BehaviourEventSource = RULEBOOK | LEGACY
  deriving (Show)

-- | Who and what an event is about — shared by every event from one evaluation.
data BehaviourEventContext = BehaviourEventContext
  { callerApp :: LYDL.CallerApp,
    merchantOperatingCityId :: Text,
    entityType :: Text,
    entityId :: Text,
    domain :: LYT.LogicDomain,
    rulebookVersion :: Maybe Int,
    actionType :: Text,
    eventSource :: BehaviourEventSource
  }

-- | Merchant short id and city name of the event's operating city. The control
-- center filters on these names, so it needs no city-id lookup of its own.
data BehaviourEventCity = BehaviourEventCity
  { merchantShortId :: Text,
    city :: Text
  }

-- | One outcome for the entity.
data BehaviourEventDetail = BehaviourEventDetail
  { detailKind :: Text, -- CONSEQUENCE | COMMUNICATION
    consequenceType :: Text, -- NUDGE, WARN, SOFT_BLOCK, … or the communication channel
    reasonTag :: Maybe Text,
    templateKey :: Maybe Text, -- overlay / notification key
    blockHours :: Maybe Int,
    detailExtra :: A.Value -- behaviour-specific params
  }

-- | Kafka message. Keys must match the ClickHouse columns exactly.
data BehaviourConsequenceEvent = BehaviourConsequenceEvent
  { event_id :: Text,
    occurred_at :: Text,
    merchant_operating_city_id :: Text,
    merchant_short_id :: Text,
    city :: Text,
    caller_app :: Text,
    entity_type :: Text,
    entity_id :: Text,
    behaviour :: Text,
    rulebook_version :: Maybe Int,
    action_type :: Text,
    kind :: Text,
    consequence_type :: Text,
    reason_tag :: Maybe Text,
    template_key :: Maybe Text,
    block_hours :: Maybe Int,
    source :: Text,
    extra :: Text
  }
  deriving (Generic, Show, ToJSON)

-- | The city lookup runs inside the fork, so it adds no time to the caller's flow.
-- If the city can't be found, the names are left empty and the event still goes out.
emitBehaviourEvents :: (MonadFlow m, CacheFlow m r) => m (Maybe BehaviourEventCity) -> BehaviourEventContext -> [BehaviourEventDetail] -> m ()
emitBehaviourEvents _ _ [] = pure ()
emitBehaviourEvents lookupCity ctx details =
  fork "behaviourConsequenceEvents" $ do
    disabled <- isEmissionDisabled
    unless disabled $ do
      eventCity <- fromMaybe (BehaviourEventCity "" "") <$> lookupCity
      now <- getCurrentTime
      forM_ details $ \detail -> do
        eventId <- generateGUID
        pushToKafka (mkEvent ctx eventCity eventId now detail) behaviourEventsTopic ctx.entityId

isEmissionDisabled :: (MonadFlow m, CacheFlow m r) => m Bool
isEmissionDisabled = do
  -- Read as JSON: `SET <key> 1` stores the number 1, `SET <key> '"1"'` the text "1"; both disable.
  mbFlag :: Maybe A.Value <-
    IM.withInMemCache [behaviourEventsDisabledKey] 60 $
      Hedis.runInMultiCloudRedisMaybeResult $ Hedis.get behaviourEventsDisabledKey
  pure $ mbFlag `elem` [Just (A.Number 1), Just (A.String "1")]

snapshotEventContext :: LYDL.CallerApp -> Id LYT.MerchantOperatingCity -> LYT.LogicDomain -> Maybe Int -> BehaviorSnapshot -> BehaviourEventContext
snapshotEventContext caller mocId logicDomain version snapshot =
  BehaviourEventContext
    { callerApp = caller,
      merchantOperatingCityId = mocId.getId,
      entityType = show snapshot.entityType,
      entityId = snapshot.entityId,
      domain = logicDomain,
      rulebookVersion = version,
      actionType = snapshot.actionType,
      eventSource = RULEBOOK
    }

-- | Everything a rulebook decided that affects the entity. Directives the
-- dispatcher can't parse are left out — they are never executed either.
outputEventDetails :: OrchestratedOutput -> [BehaviourEventDetail]
outputEventDetails output =
  mapMaybe consequenceDetail output.consequences <> mapMaybe communicationDetail output.communications

consequenceDetail :: CET.ConsequenceDirective -> Maybe BehaviourEventDetail
consequenceDetail directive = do
  action <- either (const Nothing) Just $ CEParser.parseDirective directive
  (tag, key, hours) <- consequenceFields action
  pure $ consequenceEventDetail (T.toUpper directive.consequenceType) tag key hours directive.params

consequenceEventDetail :: Text -> Maybe Text -> Maybe Text -> Maybe Int -> A.Value -> BehaviourEventDetail
consequenceEventDetail cType tag key hours params =
  BehaviourEventDetail
    { detailKind = "CONSEQUENCE",
      consequenceType = cType,
      reasonTag = tag,
      templateKey = key,
      blockHours = hours,
      detailExtra = params
    }

-- | (reason tag, template key, block hours). Nothing for internal bookkeeping
-- (counter increments, no-ops) — those are not outcomes for the driver.
consequenceFields :: CET.ConsequenceAction -> Maybe (Maybe Text, Maybe Text, Maybe Int)
consequenceFields = \case
  CET.NoAction -> Nothing
  CET.IncrementCounter _ -> Nothing
  CET.Nudge p -> Just (Nothing, Just p.nudgeKey, Nothing)
  CET.Warn p -> Just (Nothing, Just p.warnKey, Nothing)
  CET.SoftBlock p -> Just (p.blockReasonTag, Nothing, Just p.blockDurationHours)
  CET.FeatureBlock p -> Just (p.blockReasonTag, Nothing, Just p.blockDurationHours)
  CET.HardBlock p -> Just (p.blockReasonTag, Nothing, Just p.blockDurationHours)
  CET.PermanentBlock p -> Just (p.blockReasonTag, Nothing, Nothing)
  CET.ChargeFee p -> Just (p.feeCooldownTag, Nothing, Nothing)
  CET.AssignTag p -> Just (Just p.tagName, Nothing, Nothing)
  CET.OptOutAutoAssign _ -> Just (Nothing, Nothing, Nothing)

communicationDetail :: CMT.CommunicationDirective -> Maybe BehaviourEventDetail
communicationDetail directive
  | channel == "NO_COMMUNICATION" = Nothing
  | otherwise =
    Just
      BehaviourEventDetail
        { detailKind = "COMMUNICATION",
          consequenceType = channel,
          reasonTag = Nothing,
          templateKey = Just directive.templateKey,
          blockHours = Nothing,
          detailExtra = directive.params
        }
  where
    channel = T.toUpper directive.channel

mkEvent :: BehaviourEventContext -> BehaviourEventCity -> Text -> UTCTime -> BehaviourEventDetail -> BehaviourConsequenceEvent
mkEvent ctx eventCity eventId now detail =
  BehaviourConsequenceEvent
    { event_id = eventId,
      occurred_at = clickhouseTimestamp now,
      merchant_operating_city_id = ctx.merchantOperatingCityId,
      merchant_short_id = eventCity.merchantShortId,
      city = eventCity.city,
      caller_app = T.toUpper (show ctx.callerApp),
      entity_type = ctx.entityType,
      entity_id = ctx.entityId,
      behaviour = domainTag ctx.domain,
      rulebook_version = ctx.rulebookVersion,
      action_type = ctx.actionType,
      kind = detail.detailKind,
      consequence_type = detail.consequenceType,
      reason_tag = detail.reasonTag,
      template_key = detail.templateKey,
      block_hours = detail.blockHours,
      source = show ctx.eventSource,
      extra = encodeToText detail.detailExtra
    }

-- | The domain's JSON tag (e.g. CANCELLATION_RATE_BEHAVIOR) — the same name the
-- dashboard uses — rather than its Show form (CANCELLATION-RATE-BEHAVIOR).
domainTag :: LYT.LogicDomain -> Text
domainTag logicDomain = case A.toJSON logicDomain of
  A.Object obj | Just (A.String tag) <- KM.lookup "tag" obj -> tag
  A.String tag -> tag
  _ -> show logicDomain

-- | UTC "YYYY-MM-DD hh:mm:ss.mmm" — parsed by ClickHouse into a DateTime64(3, 'UTC') column.
clickhouseTimestamp :: UTCTime -> Text
clickhouseTimestamp now =
  T.pack (Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S" now) <> "." <> T.justifyRight 3 '0' (show millis)
  where
    millis :: Integer
    millis = (diffTimeToPicoseconds (utctDayTime now) `div` 1000000000) `mod` 1000
