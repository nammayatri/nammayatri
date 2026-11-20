module AccountHealthTests (tests) where

import "dynamic-offer-driver-app" API.Types.UI.DriverAccountHealth (BehaviourRule (..), RuleCondition (..))
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import "dynamic-offer-driver-app" SharedLogic.BehaviourManagement.ConsequenceDispatcher (parseBlockReasonFlag)
import "dynamic-offer-driver-app" SharedLogic.BehaviourManagement.RulebookSpec (decodeRules)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import "dynamic-offer-driver-app" Tools.Error (BlockReasonFlag (..))
import Prelude

tests :: TestTree
tests =
  testGroup
    "AccountHealth rulebook"
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
        fmap length (rules "note\n#rulebook-builder:{\"v\":2,\"domain\":\"X\",\"counting\":null,\"ladder\":[]}") @?= Nothing
    ]

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

-- | A NUDGE with this one condition.
ruleWhen :: Text -> Text
ruleWhen condition = "{\"name\":\"n\",\"when\":[" <> condition <> "],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"K\",\"placeholders\":[]},\"notify\":null}"

-- | Description of CANCELLATION_RATE_BEHAVIOR v6 as saved in prod (live in Bangalore and Kochi).
v6 :: Text
v6 = "Fixed overlay time in warning\n#rulebook-builder:{\"v\":1,\"domain\":\"CANCELLATION_RATE_BEHAVIOR\",\"counting\":null,\"ladder\":[{\"name\":\"Weekly block\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":60},{\"fact\":\"counters.weekly.eligibleCount\",\"op\":\">=\",\"value\":20},{\"fact\":\"counters.weekly.actionCount\",\"op\":\">=\",\"value\":12}],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":24,\"reason\":\"High weekly cancellation rate\",\"reasonTag\":\"CancellationRateWeekly\",\"cooldownHours\":168},\"notify\":null},{\"name\":\"Daily block\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":65},{\"fact\":\"counters.daily.eligibleCount\",\"op\":\">=\",\"value\":7},{\"fact\":\"counters.daily.actionCount\",\"op\":\">=\",\"value\":6}],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":12,\"reason\":\"High daily cancellation rate\",\"reasonTag\":\"CancellationRateDaily\",\"cooldownHours\":24},\"notify\":null},{\"name\":\"Weekly Warn\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":35},{\"fact\":\"counters.weekly.eligibleCount\",\"op\":\">=\",\"value\":10},{\"fact\":\"counters.weekly.actionCount\",\"op\":\">=\",\"value\":5}],\"action\":{\"kind\":\"WARN\",\"overlayKey\":\"CANCELLATION_RATE_WARN_WEEKLY\",\"placeholders\":[{\"name\":\"WeeklyOffencesCount\",\"fact\":\"counters.weekly.actionCount\"},{\"name\":\"WeeklyCountedEvents\",\"fact\":\"counters.weekly.eligibleCount\"}],\"showOnProfile\":true,\"profileWarningHours\":168},\"notify\":null},{\"name\":\"Daily Warn\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":50},{\"fact\":\"counters.daily.eligibleCount\",\"op\":\">=\",\"value\":4},{\"fact\":\"counters.daily.actionCount\",\"op\":\">=\",\"value\":3}],\"action\":{\"kind\":\"WARN\",\"overlayKey\":\"CANCELLATION_RATE_WARN_DAILY\",\"placeholders\":[{\"name\":\"DailyOffencesCount\",\"fact\":\"counters.daily.actionCount\"},{\"name\":\"DailyCountedEvents\",\"fact\":\"counters.daily.eligibleCount\"}],\"showOnProfile\":true,\"profileWarningHours\":24},\"notify\":null},{\"name\":\"Weekly Info\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":25},{\"fact\":\"counters.weekly.eligibleCount\",\"op\":\">=\",\"value\":5},{\"fact\":\"counters.weekly.actionCount\",\"op\":\">=\",\"value\":3}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_NUDGE_WEEKLY\",\"placeholders\":[{\"name\":\"WeeklyOffencesCount\",\"fact\":\"counters.weekly.actionCount\"},{\"name\":\"WeeklyCountedEvents\",\"fact\":\"counters.weekly.eligibleCount\"}]},\"notify\":null},{\"name\":\"Daily Info\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":30},{\"fact\":\"counters.daily.eligibleCount\",\"op\":\">=\",\"value\":4},{\"fact\":\"counters.daily.actionCount\",\"op\":\">=\",\"value\":2}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_NUDGE_DAILY\",\"placeholders\":[{\"name\":\"DailyOffencesCount\",\"fact\":\"counters.daily.actionCount\"},{\"name\":\"DailyCountedEvents\",\"fact\":\"counters.daily.eligibleCount\"}]},\"notify\":null}]}"

-- | Description of CANCELLATION_RATE_BEHAVIOR v3 as saved in prod: its warnings were nudges.
v3 :: Text
v3 = "V_3\n#rulebook-builder:{\"v\":1,\"domain\":\"CANCELLATION_RATE_BEHAVIOR\",\"counting\":null,\"ladder\":[{\"name\":\"Weekly block\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":50},{\"fact\":\"counters.weekly.eligibleCount\",\"op\":\">=\",\"value\":15},{\"fact\":\"counters.weekly.actionCount\",\"op\":\">=\",\"value\":9}],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":24,\"reason\":\"High weekly cancellation rate\",\"reasonTag\":\"CancellationRateWeekly\",\"cooldownHours\":168},\"notify\":null},{\"name\":\"Daily block\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":60},{\"fact\":\"counters.daily.actionCount\",\"op\":\">=\",\"value\":5}],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":12,\"reason\":\"High daily cancellation rate\",\"reasonTag\":\"CancellationRateDaily\",\"cooldownHours\":24},\"notify\":null},{\"name\":\"Weekly Warn\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":35},{\"fact\":\"counters.weekly.eligibleCount\",\"op\":\">=\",\"value\":10},{\"fact\":\"counters.weekly.actionCount\",\"op\":\"==\",\"value\":7}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_WARN_WEEKLY\",\"placeholders\":[{\"name\":\"WeeklyOffencesCount\",\"fact\":\"counters.weekly.actionCount\"},{\"name\":\"WeeklyCountedEvents\",\"fact\":\"counters.weekly.eligibleCount\"}]},\"notify\":null},{\"name\":\"Daily Warn\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":50},{\"fact\":\"counters.daily.actionCount\",\"op\":\"==\",\"value\":4}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_WARN_DAILY\",\"placeholders\":[{\"name\":\"DailyOffencesCount\",\"fact\":\"counters.daily.actionCount\"},{\"name\":\"DailyCountedEvents\",\"fact\":\"counters.daily.eligibleCount\"}]},\"notify\":null},{\"name\":\"Weekly Info\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":25},{\"fact\":\"counters.weekly.actionCount\",\"op\":\"==\",\"value\":4}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_NUDGE_WEEKLY\",\"placeholders\":[{\"name\":\"WeeklyOffencesCount\",\"fact\":\"counters.weekly.actionCount\"},{\"name\":\"WeeklyCountedEvents\",\"fact\":\"counters.weekly.eligibleCount\"}]},\"notify\":null},{\"name\":\"Daily Info\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":30},{\"fact\":\"counters.daily.actionCount\",\"op\":\"==\",\"value\":2}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_NUDGE_DAILY\",\"placeholders\":[{\"name\":\"DailyOffencesCount\",\"fact\":\"counters.daily.actionCount\"},{\"name\":\"DailyCountedEvents\",\"fact\":\"counters.daily.eligibleCount\"}]},\"notify\":null}]}"
