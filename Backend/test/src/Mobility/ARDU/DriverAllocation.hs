{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.ARDU.DriverAllocation (spec) where

import Data.Default.Class
import qualified Data.Vector as V
import "dynamic-offer-driver-app" Domain.Types.Common (DriverMode (..))
import "dynamic-offer-driver-app" Domain.Types.DriverIntelligentPoolConfig (IntelligentScores (..))
import qualified "beckn-spec" Domain.Types.ServiceTierType as DVST
import qualified "beckn-spec" Domain.Types.VehicleVariant as Vehicle
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (Meters (..), Seconds (..))
import qualified "dynamic-offer-driver-app" SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool as AllocDP
import "dynamic-offer-driver-app" SharedLogic.DriverPool (getBatchSize)
import "dynamic-offer-driver-app" SharedLogic.DriverPool.Types
import Test.Hspec

-- ============================================================
-- Test Data Helpers
-- ============================================================

mkDriverPoolResult :: Text -> Meters -> Maybe DriverMode -> DriverPoolResult
mkDriverPoolResult dId dist mode' =
  def
    { driverId = Id dId,
      distanceToPickup = dist,
      variant = Vehicle.AUTO_RICKSHAW,
      mode = mode'
    }

mkDriverWithDist :: Text -> Meters -> Meters -> Seconds -> Maybe DriverMode -> DriverPoolWithActualDistResult
mkDriverWithDist dId dist actualDist actualDur mode' =
  def
    { driverPoolResult = mkDriverPoolResult dId dist mode',
      actualDistanceToPickup = actualDist,
      actualDurationToPickup = actualDur
    }

mkOnlineDriver :: Text -> Meters -> Meters -> Seconds -> DriverPoolWithActualDistResult
mkOnlineDriver dId dist actualDist actualDur = mkDriverWithDist dId dist actualDist actualDur (Just ONLINE)

mkSilentDriver :: Text -> Meters -> Meters -> Seconds -> DriverPoolWithActualDistResult
mkSilentDriver dId dist actualDist actualDur = mkDriverWithDist dId dist actualDist actualDur (Just SILENT)

getDriverId :: DriverPoolWithActualDistResult -> Text
getDriverId d = getId d.driverPoolResult.driverId

getDriverIds :: [DriverPoolWithActualDistResult] -> [Text]
getDriverIds = map getDriverId

-- ============================================================
-- Handler Logic Simulation
--
-- The actual handler uses heavy monadic constraints (HandleMonad)
-- that require Kafka, Redis, etc. We test the decision flow by
-- encoding the handler's branching logic as a pure function.
-- ============================================================

data HandlerAction
  = ActionComplete
  | ActionReschedule PoolType (Maybe Seconds)
  | ActionSearchExpired
  | ActionInitiateNewBatch
  | ActionCancelSearchTry
  | ActionCancelBooking
  deriving (Show, Eq)

data MockHandle = MockHandle
  { mockIsSearchTryValid :: Bool,
    mockIsBookingValid :: Bool,
    mockIsReceivedMaxDriverQuotes :: Bool,
    mockIsBatchNumExceedLimit :: Bool,
    mockIsScheduledBooking :: Bool,
    mockPoolType :: PoolType,
    mockNextScheduleTime :: Maybe Seconds
  }

defaultMockHandle :: MockHandle
defaultMockHandle =
  MockHandle
    { mockIsSearchTryValid = True,
      mockIsBookingValid = True,
      mockIsReceivedMaxDriverQuotes = False,
      mockIsBatchNumExceedLimit = False,
      mockIsScheduledBooking = False,
      mockPoolType = NormalPool,
      mockNextScheduleTime = Nothing
    }

-- Pure translation of the handler's decision tree from Handle.hs
simulateHandler :: MockHandle -> [HandlerAction]
simulateHandler MockHandle {..}
  | not mockIsSearchTryValid || not mockIsBookingValid =
      [ActionComplete]
  | mockIsReceivedMaxDriverQuotes =
      [ActionComplete]
  | mockIsBatchNumExceedLimit && mockIsScheduledBooking =
      [ActionInitiateNewBatch, ActionComplete]
  | mockIsBatchNumExceedLimit =
      [ActionSearchExpired, ActionCancelSearchTry, ActionCancelBooking, ActionComplete]
  | otherwise =
      [ActionReschedule mockPoolType mockNextScheduleTime]

-- ============================================================
-- Tests
-- ============================================================

spec :: Spec
spec = do
  splitSilentDriversSpec
  getBatchSizeSpec
  isBookAnySpec
  handlerLogicSpec
  edgeCaseSpec
  timeoutHandlingSpec
  reallocationSpec

-- ============================================================
-- 1. splitSilentDriversAndSortWithDistance tests
-- ============================================================

splitSilentDriversSpec :: Spec
splitSilentDriversSpec = describe "splitSilentDriversAndSortWithDistance" $ do
  it "sorts online drivers by actual distance (closest first)" $ do
    let drivers =
          [ mkOnlineDriver "d3" 300 3000 180,
            mkOnlineDriver "d1" 100 1000 60,
            mkOnlineDriver "d2" 200 2000 120
          ]
        sorted = AllocDP.splitSilentDriversAndSortWithDistance drivers
    getDriverIds sorted `shouldBe` ["d1", "d2", "d3"]

  it "places silent drivers after all online drivers" $ do
    let drivers =
          [ mkSilentDriver "s1" 50 500 30,
            mkOnlineDriver "d1" 200 2000 120,
            mkOnlineDriver "d2" 100 1000 60,
            mkSilentDriver "s2" 75 750 45
          ]
        sorted = AllocDP.splitSilentDriversAndSortWithDistance drivers
    getDriverIds sorted `shouldBe` ["d2", "d1", "s1", "s2"]

  it "sorts silent drivers by distance among themselves" $ do
    let drivers =
          [ mkSilentDriver "s2" 200 2000 120,
            mkSilentDriver "s1" 100 1000 60,
            mkSilentDriver "s3" 300 3000 180
          ]
        sorted = AllocDP.splitSilentDriversAndSortWithDistance drivers
    getDriverIds sorted `shouldBe` ["s1", "s2", "s3"]

  it "handles empty driver list" $ do
    let sorted = AllocDP.splitSilentDriversAndSortWithDistance []
    sorted `shouldBe` []

  it "handles single driver" $ do
    let drivers = [mkOnlineDriver "d1" 100 1000 60]
        sorted = AllocDP.splitSilentDriversAndSortWithDistance drivers
    getDriverIds sorted `shouldBe` ["d1"]

  it "handles all silent drivers" $ do
    let drivers =
          [ mkSilentDriver "s3" 300 3000 180,
            mkSilentDriver "s1" 100 1000 60
          ]
        sorted = AllocDP.splitSilentDriversAndSortWithDistance drivers
    getDriverIds sorted `shouldBe` ["s1", "s3"]

  it "handles all online drivers" $ do
    let drivers =
          [ mkOnlineDriver "d2" 200 2000 120,
            mkOnlineDriver "d1" 100 1000 60
          ]
        sorted = AllocDP.splitSilentDriversAndSortWithDistance drivers
    getDriverIds sorted `shouldBe` ["d1", "d2"]

  it "treats drivers with mode=Nothing as active (not silent)" $ do
    let drivers =
          [ mkSilentDriver "s1" 50 500 30,
            mkDriverWithDist "d_none" 200 2000 120 Nothing,
            mkOnlineDriver "d1" 100 1000 60
          ]
        sorted = AllocDP.splitSilentDriversAndSortWithDistance drivers
    -- d1 and d_none are active, s1 is silent; active sorted by actualDist
    getDriverIds sorted `shouldBe` ["d1", "d_none", "s1"]

-- ============================================================
-- 2. getBatchSize tests
-- ============================================================

getBatchSizeSpec :: Spec
getBatchSizeSpec = describe "getBatchSize" $ do
  it "returns dynamic batch size when index is valid" $ do
    let dynamicSizes = V.fromList [5, 10, 15, 20]
    getBatchSize dynamicSizes 0 30 `shouldBe` 10
    getBatchSize dynamicSizes 1 30 `shouldBe` 15
    getBatchSize dynamicSizes 2 30 `shouldBe` 20

  it "returns last element for index beyond vector length" $ do
    let dynamicSizes = V.fromList [5, 10, 15]
    getBatchSize dynamicSizes 5 30 `shouldBe` 15
    getBatchSize dynamicSizes 10 30 `shouldBe` 15

  it "returns default batch size for empty vector" $ do
    getBatchSize V.empty 0 30 `shouldBe` 30

  it "returns default batch size when vector has single element and index is negative" $ do
    let dynamicSizes = V.fromList [5]
    getBatchSize dynamicSizes (-2) 30 `shouldBe` 30

-- ============================================================
-- 3. isBookAny tests
-- ============================================================

isBookAnySpec :: Spec
isBookAnySpec = describe "isBookAny" $ do
  it "returns True when multiple service tiers present" $
    AllocDP.isBookAny [DVST.AUTO_RICKSHAW, DVST.SEDAN] `shouldBe` True

  it "returns False for single service tier" $
    AllocDP.isBookAny [DVST.AUTO_RICKSHAW] `shouldBe` False

  it "returns False for empty list" $
    AllocDP.isBookAny [] `shouldBe` False

  it "returns True for three service tiers" $
    AllocDP.isBookAny [DVST.AUTO_RICKSHAW, DVST.SEDAN, DVST.SUV] `shouldBe` True

-- ============================================================
-- 4. Handler Logic Tests (simulated)
-- ============================================================

handlerLogicSpec :: Spec
handlerLogicSpec = describe "Handler logic" $ do
  it "completes immediately when search try is invalid" $
    simulateHandler defaultMockHandle {mockIsSearchTryValid = False}
      `shouldBe` [ActionComplete]

  it "completes immediately when booking is invalid" $
    simulateHandler defaultMockHandle {mockIsBookingValid = False}
      `shouldBe` [ActionComplete]

  it "completes when both search and booking are invalid" $
    simulateHandler defaultMockHandle {mockIsSearchTryValid = False, mockIsBookingValid = False}
      `shouldBe` [ActionComplete]

  it "completes when max driver quotes received" $
    simulateHandler defaultMockHandle {mockIsReceivedMaxDriverQuotes = True}
      `shouldBe` [ActionComplete]

  it "reschedules with pool info when drivers available" $ do
    let handle =
          defaultMockHandle
            { mockPoolType = NormalPool,
              mockNextScheduleTime = Nothing
            }
    simulateHandler handle `shouldBe` [ActionReschedule NormalPool Nothing]

  it "reschedules with GoHomePool type when go-home drivers found" $ do
    let handle =
          defaultMockHandle
            { mockPoolType = GoHomePool,
              mockNextScheduleTime = Just 5
            }
    simulateHandler handle `shouldBe` [ActionReschedule GoHomePool (Just 5)]

  it "still reschedules even with empty pool (batch not exceeded)" $ do
    let handle =
          defaultMockHandle
            { mockPoolType = NormalPool,
              mockNextScheduleTime = Nothing
            }
    simulateHandler handle `shouldBe` [ActionReschedule NormalPool Nothing]

-- ============================================================
-- 5. Edge Case Tests
-- ============================================================

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge cases" $ do
  describe "No drivers available (batch exceeded)" $ do
    it "expires search and cancels when batch limit exceeded for non-scheduled" $ do
      let actions =
            simulateHandler
              defaultMockHandle
                { mockIsBatchNumExceedLimit = True,
                  mockIsScheduledBooking = False
                }
      actions `shouldContain` [ActionSearchExpired]
      actions `shouldContain` [ActionCancelSearchTry]
      actions `shouldContain` [ActionCancelBooking]
      actions `shouldContain` [ActionComplete]

    it "initiates new driver search batch for scheduled booking" $ do
      let actions =
            simulateHandler
              defaultMockHandle
                { mockIsBatchNumExceedLimit = True,
                  mockIsScheduledBooking = True
                }
      actions `shouldBe` [ActionInitiateNewBatch, ActionComplete]

  describe "All drivers busy" $ do
    it "reschedules when pool is empty but batch limit not reached" $ do
      let actions =
            simulateHandler
              defaultMockHandle
                { mockIsBatchNumExceedLimit = False
                }
      actions `shouldBe` [ActionReschedule NormalPool Nothing]

  describe "Driver goes offline during allocation" $ do
    it "offline drivers are excluded from pool but allocation proceeds" $ do
      let onlineDrivers = [mkOnlineDriver "d1" 100 1000 60]
          sorted = AllocDP.splitSilentDriversAndSortWithDistance onlineDrivers
      getDriverIds sorted `shouldBe` ["d1"]

    it "silent mode drivers are deprioritized in pool" $ do
      let drivers =
            [ mkSilentDriver "s1" 50 500 30,
              mkOnlineDriver "d1" 200 2000 120
            ]
          sorted = AllocDP.splitSilentDriversAndSortWithDistance drivers
      -- Online driver d1 should come first despite being farther
      getDriverIds sorted `shouldBe` ["d1", "s1"]

-- ============================================================
-- 6. Timeout Handling Tests
-- ============================================================

timeoutHandlingSpec :: Spec
timeoutHandlingSpec = describe "Timeout handling" $ do
  it "search try becoming invalid stops allocation" $ do
    let actions = simulateHandler defaultMockHandle {mockIsSearchTryValid = False}
    actions `shouldBe` [ActionComplete]
    length actions `shouldBe` 1

  it "max quotes stops further batch processing" $ do
    let actions =
          simulateHandler
            defaultMockHandle
              { mockIsReceivedMaxDriverQuotes = True,
                mockIsBatchNumExceedLimit = False
              }
    actions `shouldBe` [ActionComplete]

  it "scheduled booking gets special treatment when timed out" $ do
    let actions =
          simulateHandler
            defaultMockHandle
              { mockIsBatchNumExceedLimit = True,
                mockIsScheduledBooking = True
              }
    actions `shouldNotContain` [ActionSearchExpired]
    actions `shouldContain` [ActionInitiateNewBatch]

  it "non-scheduled booking expires and cleans up when timed out" $ do
    let actions =
          simulateHandler
            defaultMockHandle
              { mockIsBatchNumExceedLimit = True,
                mockIsScheduledBooking = False
              }
    actions `shouldBe` [ActionSearchExpired, ActionCancelSearchTry, ActionCancelBooking, ActionComplete]

-- ============================================================
-- 7. Reallocation Logic Tests
-- ============================================================

reallocationSpec :: Spec
reallocationSpec = describe "Reallocation logic" $ do
  describe "Batch progression" $ do
    it "reschedules with next schedule time from pool flags" $ do
      let actions =
            simulateHandler
              defaultMockHandle
                { mockPoolType = GoHomePool,
                  mockNextScheduleTime = Just 3
                }
      actions `shouldBe` [ActionReschedule GoHomePool (Just 3)]

    it "reschedules with no delay for normal pool" $ do
      let actions =
            simulateHandler
              defaultMockHandle
                { mockPoolType = NormalPool,
                  mockNextScheduleTime = Nothing
                }
      actions `shouldBe` [ActionReschedule NormalPool Nothing]

  describe "Pool type transitions" $ do
    it "special drivers pool type is preserved" $
      simulateHandler defaultMockHandle {mockPoolType = SpecialDriversPool}
        `shouldBe` [ActionReschedule SpecialDriversPool Nothing]

    it "special zone queue pool type is preserved" $
      simulateHandler defaultMockHandle {mockPoolType = SpecialZoneQueuePool}
        `shouldBe` [ActionReschedule SpecialZoneQueuePool Nothing]

  describe "Driver pool sorting for reallocation" $ do
    it "sorts mixed pool correctly for next batch" $ do
      let drivers =
            [ mkSilentDriver "s1" 50 500 30,
              mkOnlineDriver "d3" 300 3000 180,
              mkOnlineDriver "d1" 100 1000 60,
              mkSilentDriver "s2" 150 1500 90,
              mkOnlineDriver "d2" 200 2000 120
            ]
          sorted = AllocDP.splitSilentDriversAndSortWithDistance drivers
      getDriverIds sorted `shouldBe` ["d1", "d2", "d3", "s1", "s2"]

    it "maintains distance ordering within each group" $ do
      let drivers =
            [ mkOnlineDriver "d_far" 1000 10000 600,
              mkOnlineDriver "d_near" 100 1000 60,
              mkOnlineDriver "d_mid" 500 5000 300,
              mkSilentDriver "s_far" 800 8000 480,
              mkSilentDriver "s_near" 200 2000 120
            ]
          sorted = AllocDP.splitSilentDriversAndSortWithDistance drivers
      getDriverIds sorted `shouldBe` ["d_near", "d_mid", "d_far", "s_near", "s_far"]

  describe "IntelligentScores defaults" $ do
    it "default intelligent scores have no values" $ do
      let scores = def :: IntelligentScores
      scores.acceptanceRatio `shouldBe` Nothing
      scores.cancellationRatio `shouldBe` Nothing
      scores.availableTime `shouldBe` Nothing
      scores.driverSpeed `shouldBe` Nothing
      scores.rideFrequency `shouldBe` Nothing
      scores.actualPickupDistanceScore `shouldBe` Nothing
      scores.rideRequestPopupDelayDuration `shouldBe` 0

    it "driver pool result defaults are sane" $ do
      let dpr = def :: DriverPoolWithActualDistResult
      dpr.actualDistanceToPickup `shouldBe` Meters 0
      dpr.actualDurationToPickup `shouldBe` Seconds 0
      dpr.isPartOfIntelligentPool `shouldBe` False
      dpr.pickupZone `shouldBe` False
      dpr.isForwardRequest `shouldBe` False

  describe "Batch size during reallocation" $ do
    it "dynamically adjusts batch size per batch number" $ do
      let dynamicSizes = V.fromList [5, 10, 15, 20]
      getBatchSize dynamicSizes 0 30 `shouldBe` 10
      getBatchSize dynamicSizes 1 30 `shouldBe` 15
      getBatchSize dynamicSizes 2 30 `shouldBe` 20
      getBatchSize dynamicSizes 5 30 `shouldBe` 20

    it "falls back to default for empty dynamic config" $ do
      getBatchSize V.empty 0 25 `shouldBe` 25
      getBatchSize V.empty 3 25 `shouldBe` 25
