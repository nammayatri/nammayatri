module AccountHealthTests (tests) where

import "dynamic-offer-driver-app" API.Types.UI.DriverAccountHealth (AccountBlock (..), AccountHealthStatus (..), ActiveBlock (..), ActiveBlockType (..), BehaviourPeriod (..), BehaviourRule (..), RuleCondition (..))
import qualified Data.Aeson as A
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import "dynamic-offer-driver-app" Domain.Action.UI.DriverAccountHealth (DriverBlock (..), accountBlock, accountHealthStatus, activeBlocks, candidateBlocks, eventsPeriod, periodsToShow)
import qualified "dynamic-offer-driver-app" Domain.Types.Common as DTCommon
import qualified "behavior-tracker" Lib.BehaviorTracker.Types as BTT
import qualified "yudhishthira" Lib.Yudhishthira.Types as LYT
import "yudhishthira" Lib.Yudhishthira.Types.AppDynamicLogicElement (AppDynamicLogicElement (..))
import "dynamic-offer-driver-app" SharedLogic.BehaviourManagement.ConsequenceDispatcher (parseBlockReasonFlag)
import "dynamic-offer-driver-app" SharedLogic.BehaviourManagement.EventsWindow (eventsWindowSizeOf, rowsSettings)
import qualified "dynamic-offer-driver-app" SharedLogic.BehaviourManagement.Packs as Packs
import "dynamic-offer-driver-app" SharedLogic.BehaviourManagement.RulebookSpec (decodeRules, hasRulebookTable)
import qualified "dynamic-offer-driver-app" SharedLogic.BehaviourManagement.Visibility as Visibility
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import "dynamic-offer-driver-app" Tools.Error (BlockReasonFlag (..))
import Prelude

tests :: TestTree
tests = testGroup "AccountHealth" [rulebookTests, eventsTests, blockTests, activeBlockTests, behaviourTests]

rulebookTests :: TestTree
rulebookTests =
  testGroup
    "rulebook"
    [ testCase "live v6: every rule in rulebook order, with its kind and hours" $
        fmap (map (\rule -> (rule.kind, rule.hours))) (rules v6)
          @?= Just [("HARD_BLOCK", Just 24), ("HARD_BLOCK", Just 12), ("WARN", Nothing), ("WARN", Nothing), ("NUDGE", Nothing), ("NUDGE", Nothing)],
      testCase "a block's tag is its reasonTag; a warning's or nudge's is its overlay key" $
        fmap (map (.tag)) (rules v6)
          @?= Just (map Just ["CancellationRateWeekly", "CancellationRateDaily", "CANCELLATION_RATE_WARN_WEEKLY", "CANCELLATION_RATE_WARN_DAILY", "CANCELLATION_RATE_NUDGE_WEEKLY", "CANCELLATION_RATE_NUDGE_DAILY"]),
      testCase "only a HARD_BLOCK carries the flag the engine sets on the account" $
        fmap (map (.blockReasonFlag)) (rules v6)
          @?= Just [Just CancellationRateWeekly, Just CancellationRateDaily, Nothing, Nothing, Nothing, Nothing],
      testCase "conditions are copied as written, the exact matches included" $
        fmap (map conditionOf . (.conditions) . (!! 3)) (rules v3)
          @?= Just [("counters.daily.rate", ">=", A.Number 50), ("counters.daily.actionCount", "==", A.Number 4)],
      testCase "a block tag that maps to a differently named flag gets that flag" $
        fmap (map (.blockReasonFlag)) (rules (ladderOf [blockRung "LOW_RATING_BLOCK"])) @?= Just [Just LowRating],
      testCase "a block tag the engine does not know gets no flag, so a dashboard block never points at it" $
        fmap (map (.blockReasonFlag)) (rules (ladderOf [blockRung "SomethingNew"])) @?= Just [Nothing],
      testCase "a non-numeric condition value is kept" $
        fmap (map conditionOf . concatMap (.conditions)) (rules (ladderOf [ruleWhen "{\"fact\":\"onRide\",\"op\":\"==\",\"value\":false}"]))
          @?= Just [("onRide", "==", A.Bool False)],
      testCase "a hand-written version has no table" $
        fmap length (rules "Relaxed block weekly and daily drivers") @?= Nothing,
      testCase "a broken table is ignored" $
        fmap length (rules "note\n#rulebook-builder:{\"v\":1,\"ladder\":[") @?= Nothing,
      testCase "another table format version is ignored" $
        fmap length (rules "note\n#rulebook-builder:{\"v\":2,\"domain\":\"X\",\"counting\":null,\"ladder\":[]}") @?= Nothing,
      testCase "a broken table is still a table (logged as unreadable); a hand-written note is not" $
        map hasRulebookTable ["note\n#rulebook-builder:{\"v\":1,\"ladder\":[", "Relaxed block weekly and daily drivers"] @?= [True, False],
      testCase "a warning keeps its overlay key as its tag even when it also has a reasonTag" $
        fmap (map (.tag)) (rules (ladderOf [actionRung "{\"kind\":\"WARN\",\"overlayKey\":\"W\",\"reasonTag\":\"X\"}"])) @?= Just [Just "W"],
      testCase "a block keeps its reasonTag as its tag even when it also has an overlay key" $
        fmap (map (.tag)) (rules (ladderOf [actionRung "{\"kind\":\"HARD_BLOCK\",\"hours\":1,\"reasonTag\":\"CancellationRateDaily\",\"overlayKey\":\"W\"}"])) @?= Just [Just "CancellationRateDaily"],
      testCase "a block rule tagged ByDashboard gets ByDashboard, the flag the engine sets for it" $
        fmap (map (.blockReasonFlag)) (rules (ladderOf [blockRung "ByDashboard"])) @?= Just [Just ByDashboard]
    ]

blockTests :: TestTree
blockTests =
  testGroup
    "account block"
    [ testCase "a timed rule block: BLOCKED until its expiry, with its flag" $
        statusAndBlock (timedBlock inAnHour CancellationRateDaily) Nothing
          @?= (BLOCKED, Just (Just inAnHour, Just CancellationRateDaily)),
      testCase "an untimed block after an old timed one: BLOCKED, without the old flag" $
        statusAndBlock noBlock {blocked = True, blockReasonFlag = Just CancellationRateDaily, blockStateModifier = Just "dashboard-user"} Nothing
          @?= (BLOCKED, Just (Nothing, Nothing)),
      testCase "an expired block not yet lifted: GOOD, as going online would lift it" $
        statusAndBlock (timedBlock anHourAgo CancellationRateDaily) Nothing
          @?= (GOOD, Nothing),
      testCase "the overcharging block alone: BLOCKED until it ends, with no flag" $
        statusAndBlock noBlock (Just inAnHour)
          @?= (BLOCKED, Just (Just inAnHour, Nothing)),
      testCase "the driver's own block is shown over the overcharging block" $
        statusAndBlock (timedBlock inTwoHours CancellationRateWeekly) (Just inAnHour)
          @?= (BLOCKED, Just (Just inTwoHours, Just CancellationRateWeekly)),
      testCase "no block: GOOD" $
        statusAndBlock noBlock Nothing @?= (GOOD, Nothing),
      testCase "a soft block in force: RESTRICTED, with no account block" $
        statusAndBlock noBlock {softBlockExpiryTime = Just inAnHour} Nothing @?= (RESTRICTED, Nothing),
      testCase "toll routes blocked: RESTRICTED" $
        statusAndBlock noBlock {tollRouteBlockedTill = Just inAnHour} Nothing @?= (RESTRICTED, Nothing),
      testCase "AC not allowed: RESTRICTED" $
        statusAndBlock noBlock {acRestricted = True} Nothing @?= (RESTRICTED, Nothing),
      testCase "an ended soft block and toll block: GOOD" $
        statusAndBlock noBlock {softBlockExpiryTime = Just anHourAgo, tollRouteBlockedTill = Just anHourAgo} Nothing @?= (GOOD, Nothing),
      testCase "blocked and restricted at once: BLOCKED" $
        statusAndBlock (timedBlock inAnHour CancellationRateDaily) {acRestricted = True} Nothing
          @?= (BLOCKED, Just (Just inAnHour, Just CancellationRateDaily))
    ]
  where
    statusAndBlock driverBlock mbOverchargingTill =
      ( accountHealthStatus now driverBlock (isJust mbOverchargingTill) [],
        (\block -> (block.blockedUntil, block.reasonFlag)) <$> accountBlock now driverBlock mbOverchargingTill
      )

activeBlockTests :: TestTree
activeBlockTests =
  testGroup
    "active blocks"
    [ testCase "an unblocked, unrestricted driver: no block, so no record is looked up" $
        length (candidateBlocks now noBlock) @?= 0,
      testCase "a framework hard block: shown with its rule's own tag and reason, and the account's end" $
        shown (timedBlock inAnHour ByDashboard) [record BTT.HARD_BLOCK "QuoteResponseBlock" (Just inAnHour) A.Null]
          @?= [(HARD, Just "QuoteResponseBlock", Just "r", Just inAnHour, Nothing)],
      testCase "a dashboard block: shown, without a tag or reason" $
        shown (timedBlock inAnHour ByDashboard) {blockStateModifier = Just "ops-user"} [record BTT.HARD_BLOCK "QuoteResponseBlock" (Just inAnHour) A.Null]
          @?= [(HARD, Nothing, Nothing, Just inAnHour, Nothing)],
      testCase "a block whose record wasn't found (unknown tag): shown, without a tag" $
        shown (timedBlock inAnHour CancellationRateDaily) [] @?= [(HARD, Nothing, Nothing, Just inAnHour, Nothing)],
      testCase "a leftover record after a dashboard unblock: nothing shown" $
        shown noBlock [record BTT.HARD_BLOCK "QuoteResponseBlock" (Just inAnHour) A.Null] @?= [],
      testCase "an older hard block record next to a newer block: the newer one's tag" $
        shown (timedBlock inTwoHours CancellationRateDaily) [record BTT.HARD_BLOCK "QuoteResponseBlock" (Just inAnHour) A.Null, record BTT.HARD_BLOCK "CancellationRateDaily" (Just inTwoHours) A.Null]
          @?= [(HARD, Just "CancellationRateDaily", Just "r", Just inTwoHours, Nothing)],
      testCase "a permanent framework block: shown with its tag and no end" $
        shown noBlock {blocked = True, blockStateModifier = Just "BehaviorManagementFramework"} [record BTT.PERMANENT_BLOCK "DrunkAndDriveViolation" Nothing A.Null]
          @?= [(PERMANENT, Just "DrunkAndDriveViolation", Just "r", Nothing, Nothing)],
      testCase "a soft block: shown with the service tiers it takes away" $
        map
          (\block -> (block.blockType, block.tag, block.serviceTiers))
          (activeBlocks soft (candidateBlocks now soft) [record BTT.SOFT_BLOCK "LOW_RATING_SOFT_BLOCK" (Just inAnHour) A.Null])
          @?= [(SOFT, Just "LOW_RATING_SOFT_BLOCK", Just [DTCommon.AUTO_RICKSHAW])],
      testCase "an ended soft block: nothing shown, even with its record still there" $
        shown noBlock {softBlockExpiryTime = Just anHourAgo} [record BTT.SOFT_BLOCK "LOW_RATING_SOFT_BLOCK" (Just inAnHour) A.Null] @?= [],
      testCase "toll routes blocked: a TOLL_ROUTES feature block with its record's tag" $
        shown noBlock {tollRouteBlockedTill = Just inAnHour} [record BTT.FEATURE_BLOCK "TOLL_ROUTES" (Just inAnHour) (A.String "TOLL_ROUTES")]
          @?= [(FEATURE, Just "TOLL_ROUTES", Just "r", Just inAnHour, Just "TOLL_ROUTES")],
      testCase "AC not allowed: an AC_USAGE feature block, ending when its record does" $
        shown noBlock {acRestricted = True} [record BTT.FEATURE_BLOCK "AC_USAGE" (Just inAnHour) (A.String "AC_USAGE")]
          @?= [(FEATURE, Just "AC_USAGE", Just "r", Just inAnHour, Just "AC_USAGE")],
      testCase "a toll record doesn't tag the AC block" $
        shown noBlock {acRestricted = True} [record BTT.FEATURE_BLOCK "TOLL_ROUTES" (Just inAnHour) (A.String "TOLL_ROUTES")]
          @?= [(FEATURE, Nothing, Nothing, Nothing, Just "AC_USAGE")],
      testCase "blocks that stop the driver going online come first" $
        map (\(blockType, _, _, _, _) -> blockType) (shown (timedBlock inTwoHours CancellationRateDaily) {tollRouteBlockedTill = Just inAnHour, acRestricted = True} [])
          @?= [HARD, FEATURE, FEATURE]
    ]
  where
    shown driverBlock records = map blockOf (activeBlocks driverBlock (candidateBlocks now driverBlock) records)
    blockOf block = (block.blockType, block.tag, block.reason, block.blockedUntil, block.feature)
    soft = noBlock {softBlockExpiryTime = Just inAnHour, softBlockStiers = Just [DTCommon.AUTO_RICKSHAW]}
    record blockType reasonTag expiresAt extraParams =
      BTT.BlockInfo {blockType, blockReason = "r", reasonTag, blockedAt = now, expiresAt, extraParams}

behaviourTests :: TestTree
behaviourTests =
  testGroup
    "behaviours"
    [ testCase "every counter a behaviour names is in the framework's counter registry" $
        [ name
          | pack <- Packs.behaviourPacks,
            Just counting <- [pack.counting],
            name <- counting.offenceCounter : maybe [] pure counting.eligibleCounter,
            name `notElem` map fst Visibility.defaultDriverDomainConfig.actionTypes
        ]
          @?= [],
      testCase "the periods the rules read" $
        periodNames (periodsToShow (rulesReading ["counters.daily.rate", "counters.weekly.eligibleCount"]) dailyWeekly) @?= ["daily", "weekly"],
      testCase "a weekly-only rulebook: only weekly" $
        periodNames (periodsToShow (rulesReading ["counters.weekly.rate"]) dailyWeekly) @?= ["weekly"],
      testCase "an events-only rulebook: no daily/weekly (its events period comes apart)" $
        periodNames (periodsToShow (rulesReading ["counters.events.actionCount"]) dailyWeekly) @?= [],
      testCase "no readable rules (a hand-written version): all the counter's periods" $
        periodNames (periodsToShow [] dailyWeekly) @?= ["daily", "weekly"],
      testCase "cancellation counts in RIDE_CANCELLATION, against its own eligible count" $
        fmap (\counting -> (counting.offenceCounter, counting.eligibleCounter)) (Packs.findPack LYT.CANCELLATION_RATE_BEHAVIOR >>= (.counting))
          @?= Just ("RIDE_CANCELLATION", Nothing),
      testCase "quote response judges rejects against the requests sent" $
        fmap (\counting -> (counting.offenceCounter, counting.eligibleCounter)) (Packs.findPack LYT.QUOTE_RESPONSE_BEHAVIOR >>= (.counting))
          @?= Just ("QUOTE_RESPONSE_REJECT", Just "QUOTE_RESPONSE")
    ]
  where
    dailyWeekly = [BTT.mkPeriodConfig "daily" 1, BTT.mkPeriodConfig "weekly" 7]
    periodNames = map (.periodName)
    rulesReading facts = [BehaviourRule {kind = "HARD_BLOCK", tag = Nothing, blockReasonFlag = Nothing, hours = Just 1, conditions = [RuleCondition {fact, op = ">=", value = A.Number 1} | fact <- facts]}]

now, inAnHour, inTwoHours, anHourAgo :: UTCTime
now = UTCTime (fromGregorian 2026 11 23) 36000
inAnHour = addUTCTime 3600 now
inTwoHours = addUTCTime 7200 now
anHourAgo = addUTCTime (-3600) now

-- | A driver with no block and no restriction.
noBlock :: DriverBlock
noBlock =
  DriverBlock
    { blocked = False,
      blockExpiryTime = Nothing,
      blockReasonFlag = Nothing,
      blockStateModifier = Nothing,
      softBlockExpiryTime = Nothing,
      softBlockStiers = Nothing,
      tollRouteBlockedTill = Nothing,
      acRestricted = False
    }

-- | A framework hard block on the account until this time, with this flag.
timedBlock :: UTCTime -> BlockReasonFlag -> DriverBlock
timedBlock until flag = noBlock {blocked = True, blockExpiryTime = Just until, blockReasonFlag = Just flag, blockStateModifier = Just "BehaviorManagementFramework"}

eventsTests :: TestTree
eventsTests =
  testGroup
    "events window"
    [ testCase "fewer events than X: all of them, with X alongside" $
        periodOf (eventsPeriod 15 "0101") @?= ("events", 2, 4, 50, Just 15),
      testCase "more events than X: only the newest X" $
        periodOf (eventsPeriod 3 "1100101") @?= ("events", 2, 3, 66, Just 3),
      testCase "an empty window (new driver, or cleared by a block) is 0 of 0" $
        periodOf (eventsPeriod 15 "") @?= ("events", 0, 0, 0, Just 15),
      testCase "a corrupt, oversized window counts as empty, as the engine counts it" $
        periodOf (eventsPeriod 15 (T.replicate 801 "1")) @?= ("events", 0, 0, 0, Just 15),
      testCase "an EVENTS version's settings give its X" $
        eventsWindowSizeOf (rowsSettings [row (Just $ A.object ["ruleType" A..= ("EVENTS" :: Text), "windowSize" A..= (15 :: Int)])]) @?= Just 15,
      testCase "a TIME version has no X" $
        eventsWindowSizeOf (rowsSettings [row (Just $ A.object ["ruleType" A..= ("TIME" :: Text)])]) @?= Nothing,
      testCase "a version saved before settings existed is TIME" $
        eventsWindowSizeOf (rowsSettings [row Nothing]) @?= Nothing,
      testCase "no live version: no X" $
        eventsWindowSizeOf (rowsSettings []) @?= Nothing,
      testCase "an EVENTS table: its rules judge counters.events, its block sets CancellationRateEvents" $
        fmap (map (\rule -> (rule.kind, rule.tag, rule.blockReasonFlag, map conditionOf rule.conditions))) (rules eventsTable)
          @?= Just
            [ ("HARD_BLOCK", Just "CancellationRateEvents", Just CancellationRateEvents, [("counters.events.actionCount", ">=", A.Number 12)]),
              ("WARN", Just "CANCELLATION_RATE_WARN_EVENTS", Nothing, [("counters.events.actionCount", ">=", A.Number 8)]),
              ("NUDGE", Just "CANCELLATION_RATE_NUDGE_EVENTS", Nothing, [("counters.events.actionCount", ">=", A.Number 5)])
            ]
    ]

-- | Every field of a period, to compare periods (the API type has no Eq).
periodOf :: BehaviourPeriod -> (Text, Int, Int, Int, Maybe Int)
periodOf period = (period.period, period.actionCount, period.eligibleCount, period.rate, period.windowSize)

-- | A rulebook row with these settings.
row :: Maybe A.Value -> AppDynamicLogicElement
row settings =
  AppDynamicLogicElement
    { description = Nothing,
      domain = LYT.CANCELLATION_RATE_BEHAVIOR,
      logic = A.Null,
      merchantId = Nothing,
      order = 0,
      patchedElement = Nothing,
      settings,
      version = 7,
      createdAt = epoch,
      updatedAt = epoch
    }
  where
    epoch = UTCTime (fromGregorian 2026 11 23) 0

-- | The control center's EVENTS template for cancellations (X = 15), as the builder saves it.
eventsTable :: Text
eventsTable = "Events\n#rulebook-builder:{\"v\":1,\"domain\":\"CANCELLATION_RATE_BEHAVIOR\",\"counting\":null,\"window\":{\"mode\":\"EVENTS\",\"size\":15},\"ladder\":[{\"name\":\"Block\",\"when\":[{\"fact\":\"counters.events.actionCount\",\"op\":\">=\",\"value\":12}],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":24,\"reason\":\"Too many cancellations in recent rides\",\"reasonTag\":\"CancellationRateEvents\",\"cooldownHours\":null},\"notify\":null},{\"name\":\"Warn\",\"when\":[{\"fact\":\"counters.events.actionCount\",\"op\":\">=\",\"value\":8}],\"action\":{\"kind\":\"WARN\",\"overlayKey\":\"CANCELLATION_RATE_WARN_EVENTS\"},\"notify\":null},{\"name\":\"Nudge\",\"when\":[{\"fact\":\"counters.events.actionCount\",\"op\":\">=\",\"value\":5}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_NUDGE_EVENTS\"},\"notify\":null}]}"

rules :: Text -> Maybe [BehaviourRule]
rules = decodeRules parseBlockReasonFlag

conditionOf :: RuleCondition -> (Text, Text, A.Value)
conditionOf condition = (condition.fact, condition.op, condition.value)

ladderOf :: [Text] -> Text
ladderOf rungs = "t\n#rulebook-builder:{\"v\":1,\"domain\":\"X\",\"counting\":null,\"ladder\":[" <> T.intercalate "," rungs <> "]}"

-- | A one-hour HARD_BLOCK on the daily rate with this reasonTag.
blockRung :: Text -> Text
blockRung reasonTag =
  "{\"name\":\"b\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":50}],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":1,\"reason\":\"r\",\"reasonTag\":\""
    <> reasonTag
    <> "\",\"cooldownHours\":null},\"notify\":null}"

-- | A rung with this action and one condition.
actionRung :: Text -> Text
actionRung action = "{\"name\":\"a\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":50}],\"action\":" <> action <> ",\"notify\":null}"

-- | A NUDGE with this one condition.
ruleWhen :: Text -> Text
ruleWhen condition = "{\"name\":\"n\",\"when\":[" <> condition <> "],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"K\",\"placeholders\":[]},\"notify\":null}"

-- | Description of CANCELLATION_RATE_BEHAVIOR v6 as saved in prod (live in Bangalore and Kochi).
v6 :: Text
v6 = "Fixed overlay time in warning\n#rulebook-builder:{\"v\":1,\"domain\":\"CANCELLATION_RATE_BEHAVIOR\",\"counting\":null,\"ladder\":[{\"name\":\"Weekly block\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":60},{\"fact\":\"counters.weekly.eligibleCount\",\"op\":\">=\",\"value\":20},{\"fact\":\"counters.weekly.actionCount\",\"op\":\">=\",\"value\":12}],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":24,\"reason\":\"High weekly cancellation rate\",\"reasonTag\":\"CancellationRateWeekly\",\"cooldownHours\":168},\"notify\":null},{\"name\":\"Daily block\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":65},{\"fact\":\"counters.daily.eligibleCount\",\"op\":\">=\",\"value\":7},{\"fact\":\"counters.daily.actionCount\",\"op\":\">=\",\"value\":6}],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":12,\"reason\":\"High daily cancellation rate\",\"reasonTag\":\"CancellationRateDaily\",\"cooldownHours\":24},\"notify\":null},{\"name\":\"Weekly Warn\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":35},{\"fact\":\"counters.weekly.eligibleCount\",\"op\":\">=\",\"value\":10},{\"fact\":\"counters.weekly.actionCount\",\"op\":\">=\",\"value\":5}],\"action\":{\"kind\":\"WARN\",\"overlayKey\":\"CANCELLATION_RATE_WARN_WEEKLY\",\"placeholders\":[{\"name\":\"WeeklyOffencesCount\",\"fact\":\"counters.weekly.actionCount\"},{\"name\":\"WeeklyCountedEvents\",\"fact\":\"counters.weekly.eligibleCount\"}],\"showOnProfile\":true,\"profileWarningHours\":168},\"notify\":null},{\"name\":\"Daily Warn\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":50},{\"fact\":\"counters.daily.eligibleCount\",\"op\":\">=\",\"value\":4},{\"fact\":\"counters.daily.actionCount\",\"op\":\">=\",\"value\":3}],\"action\":{\"kind\":\"WARN\",\"overlayKey\":\"CANCELLATION_RATE_WARN_DAILY\",\"placeholders\":[{\"name\":\"DailyOffencesCount\",\"fact\":\"counters.daily.actionCount\"},{\"name\":\"DailyCountedEvents\",\"fact\":\"counters.daily.eligibleCount\"}],\"showOnProfile\":true,\"profileWarningHours\":24},\"notify\":null},{\"name\":\"Weekly Info\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":25},{\"fact\":\"counters.weekly.eligibleCount\",\"op\":\">=\",\"value\":5},{\"fact\":\"counters.weekly.actionCount\",\"op\":\">=\",\"value\":3}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_NUDGE_WEEKLY\",\"placeholders\":[{\"name\":\"WeeklyOffencesCount\",\"fact\":\"counters.weekly.actionCount\"},{\"name\":\"WeeklyCountedEvents\",\"fact\":\"counters.weekly.eligibleCount\"}]},\"notify\":null},{\"name\":\"Daily Info\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":30},{\"fact\":\"counters.daily.eligibleCount\",\"op\":\">=\",\"value\":4},{\"fact\":\"counters.daily.actionCount\",\"op\":\">=\",\"value\":2}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_NUDGE_DAILY\",\"placeholders\":[{\"name\":\"DailyOffencesCount\",\"fact\":\"counters.daily.actionCount\"},{\"name\":\"DailyCountedEvents\",\"fact\":\"counters.daily.eligibleCount\"}]},\"notify\":null}]}"

-- | Description of CANCELLATION_RATE_BEHAVIOR v3 as saved in prod: its warnings were nudges.
v3 :: Text
v3 = "V_3\n#rulebook-builder:{\"v\":1,\"domain\":\"CANCELLATION_RATE_BEHAVIOR\",\"counting\":null,\"ladder\":[{\"name\":\"Weekly block\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":50},{\"fact\":\"counters.weekly.eligibleCount\",\"op\":\">=\",\"value\":15},{\"fact\":\"counters.weekly.actionCount\",\"op\":\">=\",\"value\":9}],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":24,\"reason\":\"High weekly cancellation rate\",\"reasonTag\":\"CancellationRateWeekly\",\"cooldownHours\":168},\"notify\":null},{\"name\":\"Daily block\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":60},{\"fact\":\"counters.daily.actionCount\",\"op\":\">=\",\"value\":5}],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":12,\"reason\":\"High daily cancellation rate\",\"reasonTag\":\"CancellationRateDaily\",\"cooldownHours\":24},\"notify\":null},{\"name\":\"Weekly Warn\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":35},{\"fact\":\"counters.weekly.eligibleCount\",\"op\":\">=\",\"value\":10},{\"fact\":\"counters.weekly.actionCount\",\"op\":\"==\",\"value\":7}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_WARN_WEEKLY\",\"placeholders\":[{\"name\":\"WeeklyOffencesCount\",\"fact\":\"counters.weekly.actionCount\"},{\"name\":\"WeeklyCountedEvents\",\"fact\":\"counters.weekly.eligibleCount\"}]},\"notify\":null},{\"name\":\"Daily Warn\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":50},{\"fact\":\"counters.daily.actionCount\",\"op\":\"==\",\"value\":4}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_WARN_DAILY\",\"placeholders\":[{\"name\":\"DailyOffencesCount\",\"fact\":\"counters.daily.actionCount\"},{\"name\":\"DailyCountedEvents\",\"fact\":\"counters.daily.eligibleCount\"}]},\"notify\":null},{\"name\":\"Weekly Info\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":25},{\"fact\":\"counters.weekly.actionCount\",\"op\":\"==\",\"value\":4}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_NUDGE_WEEKLY\",\"placeholders\":[{\"name\":\"WeeklyOffencesCount\",\"fact\":\"counters.weekly.actionCount\"},{\"name\":\"WeeklyCountedEvents\",\"fact\":\"counters.weekly.eligibleCount\"}]},\"notify\":null},{\"name\":\"Daily Info\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":30},{\"fact\":\"counters.daily.actionCount\",\"op\":\"==\",\"value\":2}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_NUDGE_DAILY\",\"placeholders\":[{\"name\":\"DailyOffencesCount\",\"fact\":\"counters.daily.actionCount\"},{\"name\":\"DailyCountedEvents\",\"fact\":\"counters.daily.eligibleCount\"}]},\"notify\":null}]}"
