module API where

import Beckn.External.Maps as Maps
import Beckn.Prelude
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Error.Throwing
import qualified Data.List.NonEmpty as NE
import Lib.LocationUpdates as API
import Lib.LocationUpdates.Internal as I
import Routes
import System.Random
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
      randomSuffix <- runIO $ randomRIO (100, 999 :: Int)
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
      deleteFirstNwaypoints = deleteFirstNwaypointsImplementation,
      interpolatePointsAndCalculateDistance = \req -> do
        res <- Maps.snapToRoad config $ SnapToRoadReq req
        pure (res.distance, res.snappedPoints),
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
  finalDistanceCalculation ih rideId testDriverId destination
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
  finalDistanceCalculation ih rideId testDriverId destination
  failed2 <- API.isDistanceCalculationFailed ih testDriverId
  liftIO $ failed2 `shouldBe` True
