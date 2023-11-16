{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API where

import qualified Data.List.NonEmpty as NE
import Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Randomizer
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import Lib.LocationUpdates as API
import Lib.LocationUpdates.Internal as I
import Routes
import Test.Tasty.Hspec
import Utils

resetRedis :: AppEnv -> Id Person -> IO ()
resetRedis appEnv driverId = runFlow "reset Redis" appEnv $ do
  deleteDistanceKey driverId
  clearLocationUpdatesImplementation driverId

apiSpec :: AppEnv -> MapsServiceConfig -> Spec
apiSpec appEnv config =
  after_ (resetRedis appEnv testDriverId) $
    describe "Testing location-updates lib API" $ do
      randomSuffix <- runIO $ getRandomInRange (100, 999 :: Int)
      let typeSuffix =
            case config of
              GoogleConfig _ -> "google"
              OSRMConfig _ -> "osrm"
              MMIConfig _ -> "mmi"
      it "should calculate correct distance for the short curvy route" $
        successFlow appEnv config 50 650 (Id $ "shortCurvyRoute" <> typeSuffix <> show randomSuffix) shortCurvyRoute
      it "should calculate correct distance for the route with far isolated point" $
        successFlow appEnv config 120 8250 (Id $ "farIsolatedPoint" <> typeSuffix <> show randomSuffix) farIsolatedPoint
      -- 8349 for snap, 8165 for osrm with radius 20
      -- snap to road adds a little detour at the journey end, it might quite correspond to this difference
      it "should handle errors while calculating distance correctly" $
        failFlow appEnv config "fail-shortCurvyRoute" shortCurvyRoute

buildTestInterpolationHandler :: MapsServiceConfig -> RideInterpolationHandler Person TestM
buildTestInterpolationHandler config = do
  RideInterpolationHandler
    { batchSize = 30,
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
      interpolatePointsAndCalculateDistance = \req -> do
        res <- Maps.snapToRoad config $ SnapToRoadReq req
        pure (res.distance, res.snappedPoints, Google),
      updateDistance = updateDistanceTest,
      wrapDistanceCalculation = wrapDistanceCalculationImplementation,
      isDistanceCalculationFailed = isDistanceCalculationFailedImplementation
    }

testDriverId :: Id Person
testDriverId = "agent007"

successFlow :: AppEnv -> MapsServiceConfig -> Double -> Double -> Id a -> LocationUpdates -> IO ()
successFlow appEnv config eps expectedDistance rideId route = runFlow "" appEnv $ do
  let origin = getFirstPoint route
      destination = getLastPoint route
  let ih = buildTestInterpolationHandler config
  initializeDistanceCalculation ih rideId testDriverId origin
  forM_ (NE.toList route) $ \updatesBatch ->
    addIntermediateRoutePoints ih rideId testDriverId updatesBatch
  finalDistanceCalculation ih rideId testDriverId destination 0 True
  failed <- API.isDistanceCalculationFailed ih testDriverId
  liftIO $ failed `shouldBe` False
  totalDistance <- checkTraveledDistance testDriverId
  liftIO $ totalDistance `shouldSatisfy` equalsEps eps expectedDistance

failingInterpolationHandler :: MapsServiceConfig -> RideInterpolationHandler Person TestM
failingInterpolationHandler config = do
  let ih = buildTestInterpolationHandler config
  ih {getWaypointsNumber = \_ -> throwError (InternalError "test")}

failFlow :: AppEnv -> MapsServiceConfig -> Id a -> LocationUpdates -> IO ()
failFlow appEnv config rideId route = runFlow "" appEnv $ do
  let origin = getFirstPoint route
      destination = getLastPoint route
  let ih = failingInterpolationHandler config
  initializeDistanceCalculation ih rideId testDriverId origin
  failed0 <- API.isDistanceCalculationFailed ih testDriverId
  liftIO $ failed0 `shouldBe` False

  forM_ (NE.toList route) $ \updatesBatch -> do
    addIntermediateRoutePoints ih rideId testDriverId updatesBatch
    failed1 <- API.isDistanceCalculationFailed ih testDriverId
    liftIO $ failed1 `shouldBe` True
  finalDistanceCalculation ih rideId testDriverId destination 0 True
  failed2 <- API.isDistanceCalculationFailed ih testDriverId
  liftIO $ failed2 `shouldBe` True
