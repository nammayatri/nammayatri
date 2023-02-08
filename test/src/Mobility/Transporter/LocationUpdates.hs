module Mobility.Transporter.LocationUpdates where

import Common (getAppBaseUrl)
import qualified Data.List.NonEmpty as NE
import qualified "beckn-transport" Domain.Types.Booking as TRB
import EulerHS.Prelude
import HSpec
import Kernel.Prelude (roundToIntegral)
import Kernel.Types.Common (HighPrecMeters, Meters)
import Kernel.Utils.Time
import Mobility.AppBackend.APICalls
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import Mobility.Transporter.APICalls
import Mobility.Transporter.Fixtures
import qualified Mobility.Transporter.Utils as Utils
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing location updates"
    . beforeAndAfter_
      ( do
          Utils.resetDriver transporterDriver1
          Utils.resetCustomer appRegistrationToken
      )
    $ do
      it "Testing location updates flow for short curvy route" $
        defaultSuccessFlow 10 680 680 locationUpdatesRoute1 clients
      it "Testing location updates for the route with far isolated point" $
        defaultSuccessFlow 800 8350 8350 locationUpdatesIsolatedPoint clients
      it "Testing location updates for the route with reversed points list" $
        revertedPointsListSuccessFlow 800 8350 8350 locationUpdatesIsolatedPoint clients
      it "Testing success flow and location updates with outdated points" $
        outdatedPointsSuccessFlow 800 3768 8350 locationUpdatesIsolatedPoint clients
      it "Testing success flow and location updates called multiple times at the same time " $
        raceConditionSuccessFlow 800 8350 8350 locationUpdatesIsolatedPoint clients

defaultSuccessFlow :: Double -> HighPrecMeters -> Meters -> LocationUpdates -> ClientEnvs -> IO ()
defaultSuccessFlow eps distance chargeableDistance updates clients = withBecknClients clients $ do
  successFlowWithLocationUpdatesHandler eps distance chargeableDistance updates $ do
    forM_ (NE.toList updates) $ \upd -> do
      updReq <- liftIO $ buildUpdateLocationRequest upd
      Utils.updateLocation transporterDriver1 updReq
      liftIO $ threadDelay waitBetweenUpdates

revertedPointsListSuccessFlow :: Double -> HighPrecMeters -> Meters -> LocationUpdates -> ClientEnvs -> IO ()
revertedPointsListSuccessFlow eps distance chargeableDistance updates clients = withBecknClients clients $ do
  successFlowWithLocationUpdatesHandler eps distance chargeableDistance updates $ do
    forM_ (NE.toList updates) $ \upd -> do
      updReq <- liftIO $ buildUpdateLocationRequest upd
      Utils.updateLocation transporterDriver1 $ NE.reverse updReq
      liftIO $ threadDelay waitBetweenUpdates

-- There was a bug, when it was possible to update location multiple times if
-- call updateLocation with outdated points
outdatedPointsSuccessFlow :: Double -> HighPrecMeters -> Meters -> LocationUpdates -> ClientEnvs -> IO ()
outdatedPointsSuccessFlow eps distance chargeableDistance updates clients = withBecknClients clients $ do
  successFlowWithLocationUpdatesHandler eps distance chargeableDistance updates $ do
    forM_ (NE.toList updates) $ \upd -> do
      updReq <- liftIO $ makePointsOutdated <$> buildUpdateLocationRequest upd
      Utils.updateLocation transporterDriver1 updReq
      liftIO $ threadDelay waitBetweenUpdates
  where
    makePointsOutdated points = fmap (\point -> point{ts = addUTCTime (negate 600) point.ts}) points

raceConditionSuccessFlow :: Double -> HighPrecMeters -> Meters -> LocationUpdates -> ClientEnvs -> IO ()
raceConditionSuccessFlow eps distance chargeableDistance updates clients = withBecknClients clients $ do
  successFlowWithLocationUpdatesHandler eps distance chargeableDistance updates $ do
    forM_ (NE.toList updates) $ \upd -> do
      void . forkMultipleThreads 5 $ do
        updReq <- liftIO $ buildUpdateLocationRequest upd
        Utils.updateLocation transporterDriver1 updReq
      liftIO $ threadDelay waitBetweenUpdates
  where
    forkMultipleThreads a f = replicateM a . liftIO $ forkIO $ withBecknClients clients f

successFlowWithLocationUpdatesHandler :: Double -> HighPrecMeters -> Meters -> LocationUpdates -> ClientsM () -> ClientsM ()
successFlowWithLocationUpdatesHandler eps distance chargeableDistance updates locationUpdatesFunc = do
  let (origin, destination, searchReq_) = searchReqFromUpdatesList updates

  Utils.setupDriver transporterDriver1 origin

  Utils.search'Confirm appRegistrationToken searchReq_ \scRes -> do
    let tBooking = scRes.bppBooking :: TRB.Booking
        bBookingId = scRes.bapBookingId

    void $ Utils.getRideInfo transporterDriver1 tBooking.id

    -- Driver Accepts a ride
    tRide <- Utils.acceptRide transporterDriver1 tBooking

    let bppRideId = tRide.id

    -- we need to update location just before we start ride
    updReq <- liftIO $ buildUpdateLocationRequest $ origin :| []
    Utils.updateLocation transporterDriver1 updReq

    --
    Utils.startRide appRegistrationToken transporterDriver1 origin tRide bBookingId

    liftIO $ threadDelay waitBetweenUpdates
    locationUpdatesFunc

    ----
    completedRideId <- Utils.endRide appRegistrationToken transporterDriver1 destination tRide bBookingId

    tRide' <- Utils.getBPPRideById bppRideId
    tRide'.traveledDistance `shouldSatisfy` equalsEps (realToFrac eps) distance
    tRide'.chargeableDistance `shouldSatisfy` (equalsEps (roundToIntegral eps) chargeableDistance . fromJust)

    -- Leave feedback
    void . callBAP $ callAppFeedback 5 completedRideId

    liftIO $ Utils.resetDriver transporterDriver1
