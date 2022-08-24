{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LocationUpdates where

import Beckn.Mock.App hiding (run)
import Beckn.Prelude
import Beckn.Storage.Hedis.Config
import qualified Beckn.Storage.Hedis.Queries as Hedis
import Beckn.Types.Id (Id (Id))
import Beckn.Types.MapSearch (LatLong (LatLong))
import Beckn.Types.MonadGuid
import Beckn.Utils.Common
import Beckn.Utils.IOLogging
import qualified Data.List.NonEmpty as NE
import Domain.Action.UI.Location.UpdateLocation
import qualified Domain.Types.Person as Person
import SharedLogic.LocationUpdates
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

locationUpdatesTree :: AppEnv -> TestTree
locationUpdatesTree appEnv =
  testProperty "Location updates property tests" $ withMaxSuccess 100 $ qcTest appEnv

-------------------------------------------------
--------------------run types--------------------
data AppEnv = AppEnv
  { loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    hedisEnv :: HedisEnv
  }

type TestM = MockM AppEnv

------------------- utility functions ---------------------

wrapTests :: (AppEnv -> IO a) -> IO a
wrapTests func = do
  withHedisEnv defaultHedisCfg ("locationUpdatesTest:" <>) $ \hedisEnv -> do
    let loggerConfig = defaultLoggerConfig {logToFile = True, prettyPrinting = True}
    withLoggerEnv loggerConfig Nothing $ \loggerEnv -> do
      let appEnv = AppEnv loggerConfig loggerEnv hedisEnv
      func appEnv

incrDistance :: Id Person.Person -> HighPrecMeters -> TestM Double
incrDistance driverId = Hedis.incrByFloat driverId.getId . getHighPrecMeters

updateDistanceTest :: Id Person.Person -> HighPrecMeters -> TestM ()
updateDistanceTest driverId dist = void $ incrDistance driverId dist

checkTraveledDistance :: Id Person.Person -> TestM Double
checkTraveledDistance driverId = incrDistance driverId 0

deleteDistanceKey :: Id Person.Person -> TestM ()
deleteDistanceKey driverId = Hedis.del driverId.getId

equalsEps :: Double -> Double -> Double -> Bool
equalsEps eps x y = abs (x - y) < eps

------------------- test handler ---------------------
-------------------(all redis functions are real)-----

testInterpolationHandler :: Integer -> RideInterpolationHandler TestM
testInterpolationHandler batchSize =
  RideInterpolationHandler
    { batchSize,
      addPoints = addPointsImplementation,
      clearPointsList = clearPointsListImplementation,
      getWaypointsNumber = getWaypointsNumberImplementation,
      getFirstNwaypoints = getFirstNwaypointsImplementation,
      deleteFirstNwaypoints = deleteFirstNwaypointsImplementation,
      interpolatePoints = pure,
      updateDistance = updateDistanceTest
    }

------------------- generating test points batches ----

groupPoints :: [Int] -> [a] -> [[a]]
groupPoints [] _ = []
groupPoints _ [] = []
groupPoints (n : ns) xs =
  let (q, w) = splitAt n xs
   in q : groupPoints ns w

generatePointsOnTheEquator :: Int -> [LatLong]
generatePointsOnTheEquator n | n <= 0 = []
generatePointsOnTheEquator n = map (LatLong 0 . fromIntegral) [0 .. (n - 1)]

segmentLength, oneLongitudeDegreeMetersOnEquator :: Double
oneLongitudeDegreeMetersOnEquator = 111194.92664455876
segmentLength = oneLongitudeDegreeMetersOnEquator

segmentsLengthN :: Int -> Double
segmentsLengthN n = fromIntegral n * segmentLength

---------QuickCheck random generation --------

newtype BatchSize = BatchSize Integer
  deriving newtype (Show)

newtype PointsNum = PointsNum Int
  deriving newtype (Show)

newtype GroupingList = GroupingList [Int]
  deriving newtype (Show)

instance Arbitrary BatchSize where
  arbitrary = batchSizeGen

instance Arbitrary PointsNum where
  arbitrary = numPointsGen

instance Arbitrary GroupingList where
  arbitrary = groupingGen

batchSizeGen :: Gen BatchSize
batchSizeGen = BatchSize <$> choose (5, 20)

numPointsGen :: Gen PointsNum
numPointsGen = PointsNum <$> choose (8, 100)

groupingGen :: Gen GroupingList
groupingGen = GroupingList . take 50 <$> infiniteListOf groupingPointGen
  where
    groupingPointGen = choose (1, 5)

------------------ debug functions ---------------------

runOnce :: Integer -> Int -> [Int] -> IO ()
runOnce batchSize numPoints groupingList =
  wrapTests $ \appEnv ->
    quickCheck $
      withMaxSuccess 1 $
        qcTest appEnv (BatchSize batchSize) (PointsNum numPoints) (GroupingList groupingList)

runNtimes :: Int -> IO ()
runNtimes num = wrapTests $ \appEnv -> quickCheck $ withMaxSuccess num $ qcTest appEnv

---------------------------------------------------------
--------------------- main test -------------------------

qcTest :: AppEnv -> BatchSize -> PointsNum -> GroupingList -> Property
qcTest appEnv (BatchSize batchSize) (PointsNum numPoints) (GroupingList groupingList) =
  classify (numPoints < fromIntegral batchSize) "Lesser points than the batch size"
    . classify (div_ > 0 && mod_ == 0) "Total number points is integer number of batches"
    . classify (div_ > 0 && mod_ == 1) "Total number is one point greater than the integer number of batches"
    $ monadic runner $ qcTest' batchSize numPoints groupingList
  where
    (div_, mod_) = numPoints `divMod` fromIntegral batchSize

    runner :: TestM Property -> Property
    runner = ioProperty . runMock appEnv

qcTest' :: Integer -> Int -> [Int] -> PropertyM TestM ()
qcTest' batchSize numPoints groupingList = do
  let ih = testInterpolationHandler batchSize
      pointsToAdd = generatePointsOnTheEquator numPoints
      groupedPoints = groupPoints groupingList pointsToAdd
  driverId <- run $ Id <$> generateGUIDText

  forM_ groupedPoints $ processPointsGroup ih driverId
  run $ recalcDistanceBatches ih True driverId

  totalDistance <- run $ checkTraveledDistance driverId
  assert $ equalsEps 100 (fromIntegral (numPoints - 1) * segmentLength) totalDistance
  run $ deleteDistanceKey driverId

processPointsGroup :: RideInterpolationHandler TestM -> Id Person.Person -> [LatLong] -> PropertyM TestM ()
processPointsGroup ih driverId pointsGroup = do
  run $ logPretty DEBUG "points group" pointsGroup
  distanceBefore <- run $ checkTraveledDistance driverId
  pointsBefore <- run $ ih.getWaypointsNumber driverId
  let currentLength = fromIntegral $ length pointsGroup :: Integer

  run $ processWaypoints ih driverId $ NE.fromList pointsGroup

  distanceAfter <- run $ checkTraveledDistance driverId
  pointsAfter <- run $ ih.getWaypointsNumber driverId
  let (div_, rem_) = divMod (pointsBefore + currentLength) ih.batchSize
      (numberOfCalculatedBatches, pointsAfterShouldBe) =
        if rem_ == 0 && div_ > 0
          then (div_ - 1, ih.batchSize)
          else (div_, rem_)
  let numberOfCalculatedSegments = numberOfCalculatedBatches * ih.batchSize
      calculatedDistance = distanceAfter - distanceBefore
      calculatedDistanceShouldBe = fromIntegral numberOfCalculatedSegments * segmentLength
  assert $ pointsAfter == pointsAfterShouldBe
  assert $ equalsEps 100 calculatedDistanceShouldBe calculatedDistance
