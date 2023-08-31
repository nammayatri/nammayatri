{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RedisAlgorithm where

import qualified Data.List.NonEmpty as NE
import Kernel.External.Maps.Types (LatLong (LatLong))
import Kernel.Prelude
import Kernel.Types.Id (Id (Id))
import Kernel.Types.MonadGuid
import Kernel.Utils.CalculateDistance
import Kernel.Utils.Common
import Lib.LocationUpdates.Internal
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Utils

locationUpdatesTree :: AppEnv -> TestTree
locationUpdatesTree appEnv =
  testProperty "Location updates property tests" $ withMaxSuccess 100 $ qcTest appEnv

------------------- test handler ---------------------
-------------------(all redis functions are real)-----

testInterpolationHandler :: Integer -> RideInterpolationHandler Person TestM
testInterpolationHandler batchSize =
  RideInterpolationHandler
    { batchSize,
      addPoints = addPointsImplementation,
      clearLocationUpdates = clearLocationUpdatesImplementation,
      getWaypointsNumber = getWaypointsNumberImplementation,
      getFirstNwaypoints = getFirstNwaypointsImplementation,
      getAllWaypoints = getAllWaypointsImplementation,
      deleteFirstNwaypoints = deleteFirstNwaypointsImplementation,
      addInterpolatedPoints = addInterpolatedPointsImplementation,
      clearInterpolatedPoints = clearInterpolatedPointsImplementation,
      getInterpolatedPoints = getInterpolatedPointsImplementation,
      expireInterpolatedPoints = expireInterpolatedPointsImplementation,
      interpolatePointsAndCalculateDistance = \pts -> pure (getRouteLinearLength pts, pts),
      updateDistance = updateDistanceTest,
      wrapDistanceCalculation = wrapDistanceCalculationImplementation,
      isDistanceCalculationFailed = isDistanceCalculationFailedImplementation
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

-- not used anywhere

-- runOnce :: Integer -> Int -> [Int] -> IO ()
-- runOnce batchSize numPoints groupingList =
--   wrapTests $ \appEnv ->
--     quickCheck $
--       withMaxSuccess 1 $
--         qcTest appEnv (BatchSize batchSize) (PointsNum numPoints) (GroupingList groupingList)

-- runNtimes :: Int -> IO ()
-- runNtimes num = wrapTests $ \appEnv -> quickCheck $ withMaxSuccess num $ qcTest appEnv

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
    runner = ioProperty . runFlow "" appEnv

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

processPointsGroup :: RideInterpolationHandler Person TestM -> Id Person -> [LatLong] -> PropertyM TestM ()
processPointsGroup ih driverId pointsGroup = do
  run $ logPretty DEBUG "points group" pointsGroup
  distanceBefore <- run $ checkTraveledDistance driverId
  pointsBefore <- run $ ih.getWaypointsNumber driverId
  let currentLength = fromIntegral $ length pointsGroup :: Integer

  run $ processWaypoints ih driverId False 0 True $ NE.fromList pointsGroup

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
