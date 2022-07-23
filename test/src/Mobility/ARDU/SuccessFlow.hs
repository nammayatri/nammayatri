module Mobility.ARDU.SuccessFlow where

import Beckn.Types.MapSearch
import Beckn.Utils.Common (threadDelaySec)
import Common
import qualified Data.List.NonEmpty as NE
import qualified "app-backend" Domain.Types.Ride as BRide
import qualified "app-backend" Domain.Types.RideBooking as AppRB
import EulerHS.Prelude
import HSpec
import qualified Mobility.ARDU.APICalls as API
import Mobility.ARDU.Fixtures
import qualified Mobility.ARDU.Utils as Utils
import Mobility.AppBackend.APICalls
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Common
import Mobility.Fixtures.Routes
import "driver-offer-bpp" Storage.Queries.DriverLocation
import qualified "driver-offer-bpp" Storage.Queries.Ride as TQRide
import qualified "app-backend" Storage.Queries.RideBooking as BQRB
import qualified "driver-offer-bpp" Storage.Queries.RideBooking as TQRB
import qualified "driver-offer-bpp" Types.API.Driver as TDriver
import "app-backend" Types.API.Quote (OfferRes (OnDemandCab))
import qualified "driver-offer-bpp" Types.API.Ride as RideAPI
import "app-backend" Types.API.Search
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl API.getDriverOfferBppBaseUrl
  describe "Successful flow, location updates" $
    afterAll_ (threadDelaySec 5) $
      after_ (Utils.resetDriver driver1) $ do
        it "Testing success flow and location updates for short curvy route" $
          successFlowWithLocationUpdates 10 680 locationUpdatesRoute1 clients
        it "Testing success flow and location updates for the route with far isolated point" $
          successFlowWithLocationUpdates 50 8350 locationUpdatesIsolatedPoint clients

searchReqFromUpdatesList :: LocationUpdates -> (LatLong, LatLong, SearchReq)
searchReqFromUpdatesList updList =
  let origin = NE.head $ NE.head updList
      destination = NE.last $ NE.last updList
      req =
        OneWaySearch $
          OneWaySearchReq
            { origin = SearchReqLocation $ NE.head $ NE.head updList,
              destination = SearchReqLocation $ NE.last $ NE.last updList
            }
   in (origin, destination, req)

waitBetweenUpdates :: Int
waitBetweenUpdates = 1e5 + 1e6 * fromIntegral timeBetweenLocationUpdates

successFlowWithLocationUpdates :: Double -> Double -> NonEmpty (NonEmpty LatLong) -> ClientEnvs -> IO ()
successFlowWithLocationUpdates eps distance updates clients = withBecknClients clients $ do
  let (origin, destination, searchReq') = searchReqFromUpdatesList updates

  Utils.setupDriver driver1 origin

  appSearchId <- Utils.search appRegistrationToken searchReq'

  (bapQuoteAPIEntity :| _) <- Utils.getOnSearchTaxiQuotesByTransporterName appRegistrationToken appSearchId bapTransporterName

  bapQuoteAPIEntity.estimatedFare `shouldSatisfy` (> 100)

  let quoteId = bapQuoteAPIEntity.id
  Utils.select appRegistrationToken quoteId

  (searchReqForDriver :| _) <- Utils.getNearbySearchRequestForDriver driver1 quoteId

  let firstDriverFare = 30.5
  Utils.offerQuote_ driver1 firstDriverFare searchReqForDriver.searchRequestId

  (selectedQuoteAPIEntity :| _) <- Utils.getSelectedQuotesByQuoteId appRegistrationToken quoteId
  let selectedQuoteId = selectedQuoteAPIEntity.id

  bRideBookingId <- Utils.initWithCheck appRegistrationToken selectedQuoteId

  (_, tRide) <- Utils.confirmWithCheck appRegistrationToken bRideBookingId

  void . callBPP $
    API.rideStart driver1.token tRide.id $
      API.buildStartRideReq tRide.otp origin

  void . pollDesc "trip started" $ do
    inprogressRBStatusResult <- callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
    inprogressRBStatusResult.rideList `shouldSatisfy` not . null
    inprogressRBStatusResult.status `shouldBe` AppRB.TRIP_ASSIGNED
    let [inprogressRide] = inprogressRBStatusResult.rideList
    inprogressRide.status `shouldBe` BRide.INPROGRESS
    return $ Just ()
  ----

  liftIO $ threadDelay waitBetweenUpdates
  forM_ (NE.toList updates) $ \upd -> do
    updReq <- liftIO $ API.buildUpdateLocationRequest upd
    void . callBPP $ API.updateLocation driver1.token updReq
    liftIO $ threadDelay waitBetweenUpdates

  liftIO $ threadDelay waitBetweenUpdates
  ----
  void . callBPP $ API.rideEnd driver1.token tRide.id $ RideAPI.EndRideReq destination
  _ <- pollDesc "ride completed" $ do
    completedRBStatusResult <- callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
    completedRBStatusResult.rideList `shouldSatisfy` not . null
    completedRBStatusResult.status `shouldBe` AppRB.COMPLETED
    let [completedRide] = completedRBStatusResult.rideList
    completedRide.status `shouldBe` BRide.COMPLETED
    return $ Just completedRide.id

  tRide' <- Utils.getBPPRideById tRide.id
  tRide'.traveledDistance.getHighPrecMeters `shouldSatisfy` equalsEps eps distance

  -- Leave feedback
  -- not yet implemented
  --  void . callBAP $ callAppFeedback 5 completedRideId

  void . callBPP $ API.setDriverOnline driver1.token False
