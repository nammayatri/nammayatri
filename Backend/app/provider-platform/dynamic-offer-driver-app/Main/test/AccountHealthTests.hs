module AccountHealthTests (tests) where

import "dynamic-offer-driver-app" API.Types.UI.DriverAccountHealth (RuleLimit (..))
import Data.Text (Text)
import qualified Data.Text as T
import "dynamic-offer-driver-app" SharedLogic.BehaviourManagement.RulebookSpec
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "AccountHealth rulebook"
    [ testCase "live v6: block limits per period" $ do
        limits blockLimit "daily" v6 @?= Just (Just 65, Just 7, Just 6, Just 12)
        limits blockLimit "weekly" v6 @?= Just (Just 60, Just 20, Just 12, Just 24),
      testCase "live v6: warn limits per period" $ do
        limits warnLimit "daily" v6 @?= Just (Just 50, Just 4, Just 3, Nothing)
        limits warnLimit "weekly" v6 @?= Just (Just 35, Just 10, Just 5, Nothing),
      testCase "v3 has no WARN rungs (its warnings are nudges), so no warn limit" $ do
        limits warnLimit "daily" v3 @?= Nothing
        limits blockLimit "daily" v3 @?= Just (Just 60, Nothing, Just 5, Just 12),
      testCase "an unknown period has no limits" $
        limits blockLimit "monthly" v6 @?= Nothing,
      testCase "a lower bound becomes the smallest whole number that passes it" $ do
        limits blockLimit "daily" (rungWith ">" "62.5") @?= Just (Just 63, Nothing, Nothing, Just 1)
        limits blockLimit "daily" (rungWith ">=" "62.5") @?= Just (Just 63, Nothing, Nothing, Just 1)
        limits blockLimit "daily" (rungWith "<" "62.5") @?= Nothing,
      testCase "a rung that blocks on cancellations alone has no rate" $
        limits blockLimit "daily" (ladderOf [blockRung 1 [cond "actionCount" ">=" "10"]]) @?= Just (Nothing, Nothing, Just 10, Just 1),
      testCase "an exact match is not a minimum" $
        limits blockLimit "daily" (ladderOf [blockRung 1 [cond "rate" ">=" "50", cond "actionCount" "==" "4"]]) @?= Just (Just 50, Nothing, Nothing, Just 1),
      testCase "several bounds on one counter give the largest" $
        limits blockLimit "daily" (ladderOf [blockRung 1 [cond "rate" ">=" "40", cond "rate" ">=" "60"]]) @?= Just (Just 60, Nothing, Nothing, Just 1),
      testCase "a hand-written version has no table" $
        fmap length (decodeLadder "Relaxed block weekly and daily drivers") @?= Nothing,
      testCase "a broken table is ignored" $
        fmap length (decodeLadder "note\n#rulebook-builder:{\"v\":1,\"ladder\":[") @?= Nothing,
      testCase "another table format version is ignored" $
        fmap length (decodeLadder "note\n#rulebook-builder:{\"v\":2,\"domain\":\"X\",\"counting\":null,\"ladder\":[]}") @?= Nothing
    ]

limits :: (Text -> [Rung] -> Maybe RuleLimit) -> Text -> Text -> Maybe (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
limits pick period description = do
  ladder <- decodeLadder description
  limit <- pick period ladder
  pure (limit.rate, limit.minRides, limit.minCancellations, limit.hours)

ladderOf :: [Text] -> Text
ladderOf rungs = "t\n#rulebook-builder:{\"v\":1,\"domain\":\"CANCELLATION_RATE_BEHAVIOR\",\"counting\":null,\"ladder\":[" <> T.intercalate "," rungs <> "]}"

blockRung :: Int -> [Text] -> Text
blockRung hours conditions =
  "{\"name\":\"b\",\"when\":[" <> T.intercalate "," conditions <> "],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":" <> T.pack (show hours) <> ",\"reason\":\"r\",\"reasonTag\":\"t\",\"cooldownHours\":null},\"notify\":null}"

-- | A condition on a daily counter.
cond :: Text -> Text -> Text -> Text
cond counter op bound = "{\"fact\":\"counters.daily." <> counter <> "\",\"op\":\"" <> op <> "\",\"value\":" <> bound <> "}"

rungWith :: Text -> Text -> Text
rungWith op bound =
  "t\n#rulebook-builder:{\"v\":1,\"domain\":\"CANCELLATION_RATE_BEHAVIOR\",\"counting\":null,\"ladder\":[{\"name\":\"b\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\""
    <> op
    <> "\",\"value\":"
    <> bound
    <> "},{\"fact\":\"onRide\",\"op\":\"==\",\"value\":false}],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":1,\"reason\":\"r\",\"reasonTag\":\"t\",\"cooldownHours\":null},\"notify\":null}]}"

-- | Description of CANCELLATION_RATE_BEHAVIOR v6 as saved in prod (live in Bangalore and Kochi).
v6 :: Text
v6 = "Fixed overlay time in warning\n#rulebook-builder:{\"v\":1,\"domain\":\"CANCELLATION_RATE_BEHAVIOR\",\"counting\":null,\"ladder\":[{\"name\":\"Weekly block\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":60},{\"fact\":\"counters.weekly.eligibleCount\",\"op\":\">=\",\"value\":20},{\"fact\":\"counters.weekly.actionCount\",\"op\":\">=\",\"value\":12}],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":24,\"reason\":\"High weekly cancellation rate\",\"reasonTag\":\"CancellationRateWeekly\",\"cooldownHours\":168},\"notify\":null},{\"name\":\"Daily block\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":65},{\"fact\":\"counters.daily.eligibleCount\",\"op\":\">=\",\"value\":7},{\"fact\":\"counters.daily.actionCount\",\"op\":\">=\",\"value\":6}],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":12,\"reason\":\"High daily cancellation rate\",\"reasonTag\":\"CancellationRateDaily\",\"cooldownHours\":24},\"notify\":null},{\"name\":\"Weekly Warn\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":35},{\"fact\":\"counters.weekly.eligibleCount\",\"op\":\">=\",\"value\":10},{\"fact\":\"counters.weekly.actionCount\",\"op\":\">=\",\"value\":5}],\"action\":{\"kind\":\"WARN\",\"overlayKey\":\"CANCELLATION_RATE_WARN_WEEKLY\",\"placeholders\":[{\"name\":\"WeeklyOffencesCount\",\"fact\":\"counters.weekly.actionCount\"},{\"name\":\"WeeklyCountedEvents\",\"fact\":\"counters.weekly.eligibleCount\"}],\"showOnProfile\":true,\"profileWarningHours\":168},\"notify\":null},{\"name\":\"Daily Warn\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":50},{\"fact\":\"counters.daily.eligibleCount\",\"op\":\">=\",\"value\":4},{\"fact\":\"counters.daily.actionCount\",\"op\":\">=\",\"value\":3}],\"action\":{\"kind\":\"WARN\",\"overlayKey\":\"CANCELLATION_RATE_WARN_DAILY\",\"placeholders\":[{\"name\":\"DailyOffencesCount\",\"fact\":\"counters.daily.actionCount\"},{\"name\":\"DailyCountedEvents\",\"fact\":\"counters.daily.eligibleCount\"}],\"showOnProfile\":true,\"profileWarningHours\":24},\"notify\":null},{\"name\":\"Weekly Info\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":25},{\"fact\":\"counters.weekly.eligibleCount\",\"op\":\">=\",\"value\":5},{\"fact\":\"counters.weekly.actionCount\",\"op\":\">=\",\"value\":3}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_NUDGE_WEEKLY\",\"placeholders\":[{\"name\":\"WeeklyOffencesCount\",\"fact\":\"counters.weekly.actionCount\"},{\"name\":\"WeeklyCountedEvents\",\"fact\":\"counters.weekly.eligibleCount\"}]},\"notify\":null},{\"name\":\"Daily Info\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":30},{\"fact\":\"counters.daily.eligibleCount\",\"op\":\">=\",\"value\":4},{\"fact\":\"counters.daily.actionCount\",\"op\":\">=\",\"value\":2}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_NUDGE_DAILY\",\"placeholders\":[{\"name\":\"DailyOffencesCount\",\"fact\":\"counters.daily.actionCount\"},{\"name\":\"DailyCountedEvents\",\"fact\":\"counters.daily.eligibleCount\"}]},\"notify\":null}]}"

-- | Description of CANCELLATION_RATE_BEHAVIOR v3 as saved in prod: its warnings were nudges.
v3 :: Text
v3 = "V_3\n#rulebook-builder:{\"v\":1,\"domain\":\"CANCELLATION_RATE_BEHAVIOR\",\"counting\":null,\"ladder\":[{\"name\":\"Weekly block\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":50},{\"fact\":\"counters.weekly.eligibleCount\",\"op\":\">=\",\"value\":15},{\"fact\":\"counters.weekly.actionCount\",\"op\":\">=\",\"value\":9}],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":24,\"reason\":\"High weekly cancellation rate\",\"reasonTag\":\"CancellationRateWeekly\",\"cooldownHours\":168},\"notify\":null},{\"name\":\"Daily block\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":60},{\"fact\":\"counters.daily.actionCount\",\"op\":\">=\",\"value\":5}],\"action\":{\"kind\":\"HARD_BLOCK\",\"hours\":12,\"reason\":\"High daily cancellation rate\",\"reasonTag\":\"CancellationRateDaily\",\"cooldownHours\":24},\"notify\":null},{\"name\":\"Weekly Warn\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":35},{\"fact\":\"counters.weekly.eligibleCount\",\"op\":\">=\",\"value\":10},{\"fact\":\"counters.weekly.actionCount\",\"op\":\"==\",\"value\":7}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_WARN_WEEKLY\",\"placeholders\":[{\"name\":\"WeeklyOffencesCount\",\"fact\":\"counters.weekly.actionCount\"},{\"name\":\"WeeklyCountedEvents\",\"fact\":\"counters.weekly.eligibleCount\"}]},\"notify\":null},{\"name\":\"Daily Warn\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":50},{\"fact\":\"counters.daily.actionCount\",\"op\":\"==\",\"value\":4}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_WARN_DAILY\",\"placeholders\":[{\"name\":\"DailyOffencesCount\",\"fact\":\"counters.daily.actionCount\"},{\"name\":\"DailyCountedEvents\",\"fact\":\"counters.daily.eligibleCount\"}]},\"notify\":null},{\"name\":\"Weekly Info\",\"when\":[{\"fact\":\"counters.weekly.rate\",\"op\":\">=\",\"value\":25},{\"fact\":\"counters.weekly.actionCount\",\"op\":\"==\",\"value\":4}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_NUDGE_WEEKLY\",\"placeholders\":[{\"name\":\"WeeklyOffencesCount\",\"fact\":\"counters.weekly.actionCount\"},{\"name\":\"WeeklyCountedEvents\",\"fact\":\"counters.weekly.eligibleCount\"}]},\"notify\":null},{\"name\":\"Daily Info\",\"when\":[{\"fact\":\"counters.daily.rate\",\"op\":\">=\",\"value\":30},{\"fact\":\"counters.daily.actionCount\",\"op\":\"==\",\"value\":2}],\"action\":{\"kind\":\"NUDGE\",\"overlayKey\":\"CANCELLATION_RATE_NUDGE_DAILY\",\"placeholders\":[{\"name\":\"DailyOffencesCount\",\"fact\":\"counters.daily.actionCount\"},{\"name\":\"DailyCountedEvents\",\"fact\":\"counters.daily.eligibleCount\"}]},\"notify\":null}]}"
