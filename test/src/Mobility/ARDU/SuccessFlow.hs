module Mobility.ARDU.SuccessFlow where

import Beckn.Types.MapSearch
import Beckn.Utils.Common (threadDelaySec)
import Common
import qualified Data.List.NonEmpty as NE
import EulerHS.Prelude
import HSpec
import qualified Mobility.ARDU.APICalls as API
import Mobility.ARDU.Fixtures
import qualified Mobility.ARDU.Utils as Utils
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Common
import Mobility.Fixtures.Routes
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl API.getDriverOfferBppBaseUrl
  describe "Successful flow, location updates" $
    afterAll_ (threadDelaySec 5) $
      after_ (Utils.resetDriver arduDriver1) $ do
        it "Testing success flow and location updates for short curvy route" $
          successFlowWithLocationUpdates 10 680 locationUpdatesRoute1 clients
        it "Testing success flow and location updates for the route with far isolated point" $
          successFlowWithLocationUpdates 50 8350 locationUpdatesIsolatedPoint clients

waitBetweenUpdates :: Int
waitBetweenUpdates = 1e5 + 1e6 * fromIntegral timeBetweenLocationUpdates

successFlowWithLocationUpdates :: Double -> Double -> NonEmpty (NonEmpty LatLong) -> ClientEnvs -> IO ()
successFlowWithLocationUpdates eps distance updates clients = withBecknClients clients $ do
  let (origin, destination, searchReq') = searchReqFromUpdatesList updates

  Utils.setupDriver arduDriver1 origin
  scRes <- Utils.search'Confirm appRegistrationToken arduDriver1 searchReq'
  let tRide = scRes.ride
      bRideBookingId = scRes.bapRideBookingId

  Utils.startRide arduDriver1 origin tRide bRideBookingId
  ----

  liftIO $ threadDelay waitBetweenUpdates
  forM_ (NE.toList updates) $ \upd -> do
    updReq <- liftIO $ API.buildUpdateLocationRequest upd
    void . callBPP $ API.updateLocation arduDriver1.token updReq
    liftIO $ threadDelay waitBetweenUpdates

  liftIO $ threadDelay waitBetweenUpdates
  ----
  Utils.endRide arduDriver1 destination tRide bRideBookingId

  tRide' <- Utils.getBPPRideById tRide.id
  tRide'.traveledDistance.getHighPrecMeters `shouldSatisfy` equalsEps eps distance

  -- Leave feedback
  -- not yet implemented
  --  void . callBAP $ callAppFeedback 5 completedRideId

  void . callBPP $ API.setDriverOnline arduDriver1.token False
