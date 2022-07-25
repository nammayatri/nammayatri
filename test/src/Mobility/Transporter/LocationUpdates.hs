module Mobility.Transporter.LocationUpdates where

import Beckn.Types.MapSearch
import Common (getAppBaseUrl)
import qualified Data.List.NonEmpty as NE
import qualified "beckn-transport" Domain.Types.Booking as TRB
import EulerHS.Prelude
import HSpec
import Mobility.AppBackend.APICalls
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import Mobility.Transporter.APICalls
import Mobility.Transporter.Fixtures
import qualified Mobility.Transporter.Utils as Utils
import Utils

-- these tests pass only when the real google maps api key is supplied
spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing location updates (these tests pass only when the real google maps api key is supplied)" $ do
    it "Testing location updates flow for short curvy route" $
      successFlowWithLocationUpdates 10 680 locationUpdatesRoute1 clients
    it "Testing location updates for the route with far isolated point" $
      successFlowWithLocationUpdates 50 8350 locationUpdatesIsolatedPoint clients

successFlowWithLocationUpdates :: Double -> Double -> NonEmpty (NonEmpty LatLong) -> ClientEnvs -> IO ()
successFlowWithLocationUpdates eps distance updates clients = withBecknClients clients $ do
  let (origin, destination, searchReq_) = searchReqFromUpdatesList updates

  Utils.setupDriver transporterDriver1 origin

  scRes <- Utils.search'Confirm appRegistrationToken transporterDriver1 searchReq_

  let tBooking = scRes.bppBooking :: TRB.Booking
      bBookingId = scRes.bapBookingId

  -- Driver Accepts a ride
  tRide <- Utils.acceptRide transporterDriver1 tBooking

  let bppRideId = tRide.id

  -- we need to update location just before we start ride
  Utils.updateLocation transporterDriver1 $ origin :| []

  --
  Utils.startRide appRegistrationToken transporterDriver1 origin tRide bBookingId

  liftIO $ threadDelay waitBetweenUpdates
  forM_ (NE.toList updates) $ \upd -> do
    Utils.updateLocation transporterDriver1 upd
    liftIO $ threadDelay waitBetweenUpdates

  ----
  completedRideId <- Utils.endRide appRegistrationToken transporterDriver1 destination tRide bBookingId

  tRide' <- Utils.getBPPRideById bppRideId
  tRide'.traveledDistance.getHighPrecMeters `shouldSatisfy` equalsEps eps distance

  -- Leave feedback
  void . callBAP $ callAppFeedback 5 completedRideId

  liftIO $ Utils.resetDriver transporterDriver1
