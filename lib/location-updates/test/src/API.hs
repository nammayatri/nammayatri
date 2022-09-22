module API where

import Beckn.Prelude
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Error.Throwing
import qualified Data.List.NonEmpty as NE
import Lib.LocationUpdates as API
import Lib.LocationUpdates.Internal as I
import Routes
import Test.Tasty.Hspec
import Types

resetRedis :: AppEnv -> Id Person -> IO ()
resetRedis appEnv driverId = runFlow "reset Redis" appEnv $ do
  deleteDistanceKey driverId
  clearLocationUpdatesImplementation driverId

apiSpec :: AppEnv -> Spec
apiSpec appEnv =
  afterAll_ (resetRedis appEnv testDriverId) $
    describe "Testing location-updates lib API" $ do
      it "should calculate correct distance for the short curvy route" $
        successFlow appEnv 10 680 "shortCurvyRoute" shortCurvyRoute
      it "should calculate correct distance for the route with far isolated point" $
        successFlow appEnv 50 9050 "farIsolatedPoint" farIsolatedPoint
      -- what is the precise distance for this route?
      -- It seemed to be different (about 8350) before (Yuri)
      it "should handle errors while calculating distance correctly" $
        failFlow appEnv "fail-shortCurvyRoute" shortCurvyRoute

testInterpolationHandler :: RideInterpolationHandler Person TestM
testInterpolationHandler =
  RideInterpolationHandler
    { batchSize = 30,
      addPoints = addPointsImplementation,
      clearLocationUpdates = clearLocationUpdatesImplementation,
      getWaypointsNumber = getWaypointsNumberImplementation,
      getFirstNwaypoints = getFirstNwaypointsImplementation,
      deleteFirstNwaypoints = deleteFirstNwaypointsImplementation,
      interpolatePoints = callSnapToRoad,
      updateDistance = updateDistanceTest,
      wrapDistanceCalculation = wrapDistanceCalculationImplementation,
      isDistanceCalculationFailed = isDistanceCalculationFailedImplementation
    }

testDriverId :: Id Person
testDriverId = "agent007"

successFlow :: AppEnv -> Double -> Double -> Id a -> LocationUpdates -> IO ()
successFlow appEnv eps expectedDistance rideId route = runFlow "" appEnv $ do
  let origin = getFirstPoint route
      destination = getLastPoint route
      ih = testInterpolationHandler
  initializeDistanceCalculation ih rideId testDriverId origin
  forM_ (NE.toList route) $ \updatesBatch ->
    addIntermediateRoutePoints ih rideId testDriverId updatesBatch
  finalDistanceCalculation ih rideId testDriverId destination
  failed <- API.isDistanceCalculationFailed ih testDriverId
  liftIO $ failed `shouldBe` False
  totalDistance <- checkTraveledDistance testDriverId
  liftIO $ totalDistance `shouldSatisfy` equalsEps eps expectedDistance

failingInterpolationHandler :: RideInterpolationHandler Person TestM
failingInterpolationHandler =
  testInterpolationHandler {getWaypointsNumber = \_ -> throwError (InternalError "test")}

failFlow :: AppEnv -> Id a -> LocationUpdates -> IO ()
failFlow appEnv rideId route = runFlow "" appEnv $ do
  let origin = getFirstPoint route
      destination = getLastPoint route
      ih = failingInterpolationHandler
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
