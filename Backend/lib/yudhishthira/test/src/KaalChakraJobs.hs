module KaalChakraJobs (kaalChakraJobsTests) where

import Kernel.Prelude
import Kernel.Utils.Common
import Lib.Yudhishthira.Event.KaalChakra.Jobs (getNextChakraTime)
import qualified Lib.Yudhishthira.Types as LYT
import Test.Tasty
import Test.Tasty.HUnit

kaalChakraJobsTests :: TestTree
kaalChakraJobsTests =
  testGroup
    "Kaal chakra job tests"
    [ getNextChakraTimeTest
    ]

data NextJobTestCase = NextJobTestCase
  { startTime :: Maybe String,
    finishTime :: String,
    dailyTime :: String,
    weeklyTime :: String,
    monthlyTime :: String,
    quarterlyTime :: String,
    message :: String
  }

nextJobTestCases :: [NextJobTestCase]
nextJobTestCases = do
  let dayTime = " 10:30:00 UTC"
      baseTime = "2025-02-11" <> dayTime
      mkTest finishTime d w m q = NextJobTestCase (Just baseTime) finishTime (d <> dayTime) (w <> dayTime) (m <> dayTime) (q <> dayTime)
      mkDefaultTest finishTime d w m q = NextJobTestCase Nothing finishTime (d <> " 19:30:00 UTC") (w <> " 20:30:00 UTC") (m <> " 21:30:00 UTC") (q <> " 22:30:00 UTC")
  [ mkTest "2025-02-11 20:05:00 UTC" "2025-02-12" "2025-02-18" "2025-03-13" "2025-05-12" "normal case",
    mkTest "2025-02-12 00:05:00 UTC" "2025-02-12" "2025-02-18" "2025-03-13" "2025-05-12" "next day case",
    mkTest "2025-02-11 10:30:00 UTC" "2025-02-12" "2025-02-18" "2025-03-13" "2025-05-12" "edge case",
    mkTest "2025-02-12 00:00:00 UTC" "2025-02-12" "2025-02-18" "2025-03-13" "2025-05-12" "edge case2",
    mkTest "2025-02-12 10:30:00 UTC" "2025-02-13" "2025-02-18" "2025-03-13" "2025-05-12" "edge case3",
    mkTest "2025-03-11 20:00:10 UTC" "2025-03-12" "2025-03-12" "2025-03-13" "2025-05-12" "many days case",
    mkTest "2025-02-10 20:30:00 UTC" "2025-02-12" "2025-02-18" "2025-03-13" "2025-05-12" "impossible case",
    mkDefaultTest "2025-02-11 23:05:00 UTC" "2025-02-12" "2025-02-18" "2025-03-13" "2025-05-12" "default normal case",
    mkDefaultTest "2025-02-11 21:05:00 UTC" "2025-02-12" "2025-02-18" "2025-03-12" "2025-05-11" "default next day case1",
    mkDefaultTest "2025-02-12 00:05:00 UTC" "2025-02-12" "2025-02-18" "2025-03-13" "2025-05-12" "default next day case2"
    ]

getNextChakraTimeTest :: TestTree
getNextChakraTimeTest = do
  testCase "Get next kaal-chakra time test" $ do
    forM_ nextJobTestCases \nextJobTestCase -> do
      startTime <- forM nextJobTestCase.startTime parseTime
      finishTime <- parseTime nextJobTestCase.finishTime
      assertEqual ("Daily job: " <> nextJobTestCase.message) (show $ getNextChakraTime startTime finishTime LYT.Daily) nextJobTestCase.dailyTime
      assertEqual ("Weekly job: " <> nextJobTestCase.message) (show $ getNextChakraTime startTime finishTime LYT.Weekly) nextJobTestCase.weeklyTime
      assertEqual ("Monthly job: " <> nextJobTestCase.message) (show $ getNextChakraTime startTime finishTime LYT.Monthly) nextJobTestCase.monthlyTime
      assertEqual ("Quarterly job: " <> nextJobTestCase.message) (show $ getNextChakraTime startTime finishTime LYT.Quarterly) nextJobTestCase.quarterlyTime

parseTime :: String -> IO UTCTime
parseTime str = maybe (assertFailure $ "Couldn't parse UTCTime: " <> str) pure (readMaybe str)
