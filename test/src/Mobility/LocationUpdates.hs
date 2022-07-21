module Mobility.LocationUpdates where

import qualified "beckn-transport" API.UI.Booking as TbeBookingAPI
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Common (getAppBaseUrl)
import qualified Data.List.NonEmpty as NE
import qualified "app-backend" Domain.Types.Booking as AppRB
import qualified "beckn-transport" Domain.Types.Booking as TRB
import qualified "app-backend" Domain.Types.Ride as BRide
import qualified "beckn-transport" Domain.Types.Ride as TRide
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import Mobility.Fixtures.Routes
import Mobility.SuccessFlow
import qualified "app-backend" Types.API.Search as AppBackend
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

searchReqFromUpdatesList :: LocationUpdates -> AppBackend.SearchReq
searchReqFromUpdatesList updList =
  AppBackend.OneWaySearch $
    AppBackend.OneWaySearchReq
      { origin = AppBackend.SearchReqLocation $ NE.head $ NE.head updList,
        destination = AppBackend.SearchReqLocation $ NE.last $ NE.last updList
      }

waitBetweenUpdates :: Int
waitBetweenUpdates = 1e5 + 1e6 * fromIntegral timeBetweenLocationUpdates

successFlowWithLocationUpdates :: Double -> Double -> NonEmpty (NonEmpty LatLong) -> ClientEnvs -> IO ()
successFlowWithLocationUpdates eps distance updates clients = withBecknClients clients $ do
  let searchReq_ = searchReqFromUpdatesList updates
  bBookingId <- doAnAppSearchByReq searchReq_

  tBooking <- pollDesc "ride booking id should exist and should be confirmed" $ do
    trb <- getBPPBooking bBookingId
    trb.status `shouldBe` TRB.CONFIRMED
    return $ Just trb

  rideInfo <-
    poll . callBPP $
      getNotificationInfo tBooking.id driverToken1
        <&> (.rideRequest)
  rideInfo.bookingId `shouldBe` tBooking.id

  -- Driver Accepts a ride
  void . callBPP $
    rideRespond tBooking.id driverToken1 $
      TbeBookingAPI.SetDriverAcceptanceReq TbeBookingAPI.ACCEPT

  tRide <- pollDesc ("ride with id=" <> tBooking.id.getId <> " should exist and should have status=NEW") $ do
    tRide <- getBPPRide tBooking.id
    tRide.status `shouldBe` TRide.NEW
    return $ Just tRide

  let bppRideId = tRide.id

  ---- we need to update location just before we start ride
  let initLoc = NE.head $ NE.head updates
      locationEps = 1e-18
  initialUpdate <- liftIO $ buildUpdateLocationRequest $ initLoc :| []
  void . callBPP $
    updateLocation driverToken1 initialUpdate
  liftIO $ threadDelay waitBetweenUpdates

  loc <- getBPPDriverLocation $ cast testDriverId1
  loc `shouldSatisfy` \l -> equalsEps eps initLoc.lat l.lat && equalsEps locationEps initLoc.lon l.lon

  ----
  void . callBPP $
    rideStart driverToken1 tRide.id $
      buildStartRideReq tRide.otp

  void . pollDesc "ride changes its status to INPROGRESS" $ do
    inprogressRBStatusResult <- callBAP (appBookingStatus bBookingId appRegistrationToken)
    inprogressRBStatusResult.rideList `shouldSatisfy` not . null
    inprogressRBStatusResult.status `shouldBe` AppRB.TRIP_ASSIGNED
    let [inprogressRide] = inprogressRBStatusResult.rideList
    inprogressRide.status `shouldBe` BRide.INPROGRESS
    return $ Just ()
  ----

  forM_ (NE.toList updates) $ \upd -> do
    updReq <- liftIO $ buildUpdateLocationRequest upd
    void . callBPP $ updateLocation driverToken1 updReq
    liftIO $ threadDelay waitBetweenUpdates

  ----
  void . callBPP $ rideEnd driverToken1 tRide.id

  completedRideId <- pollDesc "ride should be completed" $ do
    completedRBStatusResult <- callBAP (appBookingStatus bBookingId appRegistrationToken)
    completedRBStatusResult.rideList `shouldSatisfy` not . null
    completedRBStatusResult.status `shouldBe` AppRB.COMPLETED
    let [completedRide] = completedRBStatusResult.rideList
    completedRide.status `shouldBe` BRide.COMPLETED
    return $ Just completedRide.id

  tRide' <- getBPPRideById bppRideId
  tRide'.traveledDistance.getHighPrecMeters `shouldSatisfy` equalsEps eps distance

  -- Leave feedback
  void . callBAP $ callAppFeedback 5 completedRideId

  void . callBPP $ setDriverOnline driverToken1 False
