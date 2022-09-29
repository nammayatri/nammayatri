module Mobility.ARDU.SuccessFlow where

import Beckn.Prelude (roundToIntegral)
import Beckn.Types.Common (HighPrecMeters, Meters)
import Beckn.Utils.Common (addUTCTime, threadDelaySec)
import Common
import qualified Data.List.NonEmpty as NE
import EulerHS.Prelude
import HSpec
import qualified Mobility.ARDU.APICalls as API
import Mobility.ARDU.Fixtures
import qualified Mobility.ARDU.Utils as Utils
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl API.getDriverOfferBppBaseUrl
  describe "Successful flow, location updates" $
    afterAll_ (threadDelaySec 5) $
      after_ (Utils.resetDriver arduDriver1) $ do
        it "Testing success flow and location updates for short curvy route" $
          defaultSuccessFlow 10 680 680 locationUpdatesRoute1 clients
        it "Testing success flow and location updates for the route with far isolated point" $
          defaultSuccessFlow 800 8350 8350 locationUpdatesIsolatedPoint clients
        it "Testing success flow and location updates with reversed points list" $
          reversedPointsListSuccessFlow 800 8350 8350 locationUpdatesIsolatedPoint clients
        it "Testing success flow and location updates with outdated points" $
          outdatedPointsSuccessFlow 800 3768 8350 locationUpdatesIsolatedPoint clients
        it "Testing success flow and location updates called multiple times at the same time " $
          raceConditionSuccessFlow 800 8350 8350 locationUpdatesIsolatedPoint clients

waitBetweenUpdates :: Int
waitBetweenUpdates = 1e5 + 1e6 * fromIntegral timeBetweenLocationUpdates

defaultSuccessFlow :: Double -> HighPrecMeters -> Meters -> LocationUpdates -> ClientEnvs -> IO ()
defaultSuccessFlow eps distance chargeableDistance updates clients = withBecknClients clients $ do
  successFlowWithLocationUpdatesHandler eps distance chargeableDistance updates $ do
    forM_ (NE.toList updates) $ \upd -> do
      updReq <- liftIO $ API.buildUpdateLocationRequest upd
      void . callBPP $ API.updateLocation arduDriver1.token updReq
      liftIO $ threadDelay waitBetweenUpdates

reversedPointsListSuccessFlow :: Double -> HighPrecMeters -> Meters -> LocationUpdates -> ClientEnvs -> IO ()
reversedPointsListSuccessFlow eps distance chargeableDistance updates clients = withBecknClients clients $ do
  successFlowWithLocationUpdatesHandler eps distance chargeableDistance updates $ do
    forM_ (NE.toList updates) $ \upd -> do
      updReq <- liftIO $ API.buildUpdateLocationRequest upd
      void . callBPP $ API.updateLocation arduDriver1.token $ NE.reverse updReq
      liftIO $ threadDelay waitBetweenUpdates

-- There was a bug, when it was possible to update location multiple times if
-- call updateLocation with outdated points
outdatedPointsSuccessFlow :: Double -> HighPrecMeters -> Meters -> LocationUpdates -> ClientEnvs -> IO ()
outdatedPointsSuccessFlow eps distance chargeableDistance updates clients = withBecknClients clients $ do
  successFlowWithLocationUpdatesHandler eps distance chargeableDistance updates $ do
    forM_ (NE.toList updates) $ \upd -> do
      updReq <- liftIO $ makePointsOutdated <$> API.buildUpdateLocationRequest upd
      void . callBPP $ API.updateLocation arduDriver1.token updReq
      liftIO $ threadDelay waitBetweenUpdates
  where
    makePointsOutdated points = fmap (\point -> point{ts = addUTCTime (negate 600) point.ts}) points

raceConditionSuccessFlow :: Double -> HighPrecMeters -> Meters -> LocationUpdates -> ClientEnvs -> IO ()
raceConditionSuccessFlow eps distance chargeableDistance updates clients = withBecknClients clients $ do
  successFlowWithLocationUpdatesHandler eps distance chargeableDistance updates $ do
    forM_ (NE.toList updates) $ \upd -> do
      void . forkMultipleThreads 5 $ do
        updReq <- liftIO $ API.buildUpdateLocationRequest upd
        void . callBPP $ API.updateLocation arduDriver1.token updReq
      liftIO $ threadDelay waitBetweenUpdates
  where
    forkMultipleThreads a f = replicateM a . liftIO $ forkIO $ withBecknClients clients f

successFlowWithLocationUpdatesHandler :: Double -> HighPrecMeters -> Meters -> LocationUpdates -> ClientsM () -> ClientsM ()
successFlowWithLocationUpdatesHandler eps distance chargeableDistance updates locationUpdatesFunc = do
  let (origin, destination, searchReq') = searchReqFromUpdatesList updates

  Utils.setupDriver arduDriver1 origin
  scRes <- Utils.search'Confirm appRegistrationToken arduDriver1 searchReq'
  let tRide = scRes.ride
      bBookingId = scRes.bapBookingId

  Utils.startRide arduDriver1 origin tRide bBookingId
  ----
  liftIO $ threadDelay waitBetweenUpdates
  locationUpdatesFunc
  liftIO $ threadDelay waitBetweenUpdates
  ----
  Utils.endRide arduDriver1 destination tRide bBookingId

  tRide' <- Utils.getBPPRideById tRide.id
  tRide'.traveledDistance `shouldSatisfy` equalsEps (realToFrac eps) distance
  tRide'.chargeableDistance `shouldSatisfy` (equalsEps (roundToIntegral eps) chargeableDistance . fromJust)

  -- Leave feedback
  -- not yet implemented
  --  void . callBAP $ callAppFeedback 5 completedRideId

  void . callBPP $ API.setDriverOnline arduDriver1.token False
