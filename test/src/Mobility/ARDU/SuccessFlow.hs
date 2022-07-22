module Mobility.ARDU.SuccessFlow where

import qualified "beckn-transport" API.UI.Booking as TbeBookingAPI
import Beckn.Types.Id
import Beckn.Types.MapSearch
--import qualified "driver-offer-bpp" Types.API.RideBooking as RideBookingAPI

import Beckn.Utils.Common (threadDelaySec)
import Common
import qualified Data.List.NonEmpty as NE
import "driver-offer-bpp" Domain.Types.Person as TPerson
import qualified "app-backend" Domain.Types.Ride as BRide
import qualified "driver-offer-bpp" Domain.Types.Ride as TRide
import qualified "app-backend" Domain.Types.RideBooking as AppRB
import qualified "app-backend" Domain.Types.RideBooking as BRB
import qualified "driver-offer-bpp" Domain.Types.RideBooking as TRB
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures.ARDU
import Mobility.Fixtures.AppBackend
import Mobility.Fixtures.Common
import Mobility.Fixtures.Routes
import "driver-offer-bpp" Storage.Queries.DriverLocation
import qualified "driver-offer-bpp" Storage.Queries.Ride as TQRide
import qualified "app-backend" Storage.Queries.RideBooking as BQRB
import qualified "driver-offer-bpp" Storage.Queries.RideBooking as TQRB
import qualified "driver-offer-bpp" Types.API.Driver as TDriver
import "app-backend" Types.API.Quote (OfferRes (OnDemandCab))
<<<<<<< HEAD
=======
import qualified "driver-offer-bpp" Types.API.Ride as RideAPI
>>>>>>> intermediate commit
import "app-backend" Types.API.Search
import Utils

getBAPRideBooking ::
  Id BRB.RideBooking ->
  ClientsM BRB.RideBooking
getBAPRideBooking bapRBId = do
  mbBRB <- liftIO $ runAppFlow "" $ BQRB.findById bapRBId
  mbBRB `shouldSatisfy` isJust
  let Just bRB = mbBRB
  return bRB

getBPPRideBooking ::
  Id BRB.RideBooking ->
  ClientsM TRB.RideBooking
getBPPRideBooking bapRBId = do
  bRB <- getBAPRideBooking bapRBId
  bRB.bppBookingId `shouldSatisfy` isJust
  let Just bppBookingId = bRB.bppBookingId
  mbTRB <- liftIO $ runARDUFlow "" $ TQRB.findById $ cast bppBookingId
  mbTRB $> () `shouldSatisfy` isJust
  let Just tRB = mbTRB
  return tRB

getBPPRide ::
  Id TRB.RideBooking ->
  ClientsM TRide.Ride
getBPPRide rideBookingId = do
  mbRide <- liftIO $ runARDUFlow "" $ TQRide.findActiveByRBId rideBookingId
  mbRide `shouldSatisfy` isJust
  return $ fromJust mbRide

getBPPRideById ::
  Id TRide.Ride ->
  ClientsM TRide.Ride
getBPPRideById rideId = do
  mbRide <- liftIO $ runARDUFlow "" $ TQRide.findById rideId
  mbRide `shouldSatisfy` isJust
  return $ fromJust mbRide

getBPPDriverLocation ::
  Id TPerson.Person ->
  ClientsM LatLong
getBPPDriverLocation driverId = do
  mbRes <- liftIO $ runARDUFlow "" $ findById driverId
  mbRes `shouldSatisfy` isJust
  let res = fromJust mbRes
  pure $
    LatLong
      { lat = res.lat,
        lon = res.lon
      }

equalsEps :: Double -> Double -> Double -> Bool
equalsEps eps x y = abs (x - y) < eps

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getDriverOfferBppBaseUrl
  describe "Successful flow, location updates" $
    afterAll_ (threadDelaySec 5) $
      after_ (withBecknClients clients $ void $ callBPP $ setDriverOnline driverToken1 False) $ do
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

setupDriver :: Text -> LatLong -> ClientsM ()
setupDriver driverToken initialPoint = do
  -- Driver sets online
  void . callBPP $ setDriverOnline driverToken True
  -- Moves driver to the pickup point
  preUpdate <- liftIO $ buildUpdateLocationRequest $ initialPoint :| []
  void . callBPP $
    updateLocation driverToken preUpdate

successFlowWithLocationUpdates :: Double -> Double -> NonEmpty (NonEmpty LatLong) -> ClientEnvs -> IO ()
successFlowWithLocationUpdates eps distance updates clients = withBecknClients clients $ do
  let (origin, destination, searchReq') = searchReqFromUpdatesList updates

  setupDriver driverToken1 origin

  -- Do an App Search
  appSearchId <-
    callBAP $
      searchServices appRegistrationToken searchReq'
        <&> (.searchId)

  -- Do a get quotes request for getting quotes to confirm ride
  (quoteAPIEntity :| _) <- pollDesc "get on_search quotes" $ do
    -- List all confirmed rides (type = RIDEORDER)
    callBAP (getQuotes appSearchId appRegistrationToken)
      <&> (.quotes)
      -- since all BPP can give quote for now we filter by orgId
      <&> mapMaybe \case
        OnDemandCab p -> Just p
        _ -> Nothing
      <&> filter (\p -> p.agencyName == bapTransporterName)
      <&> nonEmpty

  -- check if calculated price is greater than 0
  quoteAPIEntity.estimatedFare `shouldSatisfy` (> 100)

  let quoteId = quoteAPIEntity.id
  void $ callBAP $ selectQuote appRegistrationToken quoteId

  (searchReqForDriver :| _) <- pollDesc "get at least one nearby search request for driver" $ do
    callBPP (getNearbySearchRequests driverToken1)
      <&> (.searchRequests)
      <&> filter (\p -> p.messageId == quoteId.getId)
      <&> nonEmpty

  void $ callBPP (offerQuote driverToken1 $ TDriver.DriverOfferReq (Just 30.5) searchReqForDriver.searchRequestId)

  (selectedQuoteAPIEntity :| _) <- pollDesc "get at least one on_select quote" $ do
    callBAP (selectList appRegistrationToken quoteId)
      <&> (.selectedQuotes)
      <&> filter (\p -> p.providerName == bapTransporterName)
      <&> nonEmpty

  let selectedQuoteId = selectedQuoteAPIEntity.id

  -- Init ride from app backend
  initResult <-
    callBAP $
      appInitRide appRegistrationToken $ mkAppInitReqSelected selectedQuoteId
  let bRideBookingId = initResult.bookingId

  void . pollDesc "init result" $ do
    initRB <- getBAPRideBooking bRideBookingId
    initRB.bppBookingId `shouldSatisfy` isJust
    return $ Just ()

  -- Confirm ride from app backend
  void . callBAP $
    appConfirmRide appRegistrationToken $ mkAppConfirmReq bRideBookingId

  void . pollDesc "ride confirmed and assigned" $
    callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
      <&> (.status)
      >>= (`shouldBe` AppRB.TRIP_ASSIGNED)
      <&> Just

  tRideBooking <- pollDesc "trip assigned" $ do
    trb <- getBPPRideBooking bRideBookingId
    trb.status `shouldBe` TRB.TRIP_ASSIGNED
    return $ Just trb

  tRide <- pollDesc "new ride" $ do
    tRide <- getBPPRide tRideBooking.id
    tRide.status `shouldBe` TRide.NEW
    return $ Just tRide

  void . callBPP $
    rideStart driverToken1 tRide.id $
      buildStartRideReq tRide.otp origin

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
    updReq <- liftIO $ buildUpdateLocationRequest upd
    void . callBPP $ updateLocation driverToken1 updReq
    liftIO $ threadDelay waitBetweenUpdates

  liftIO $ threadDelay waitBetweenUpdates
  ----
  void . callBPP $ rideEnd driverToken1 tRide.id $ RideAPI.EndRideReq destination
  _ <- pollDesc "ride completed" $ do
    completedRBStatusResult <- callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
    completedRBStatusResult.rideList `shouldSatisfy` not . null
    completedRBStatusResult.status `shouldBe` AppRB.COMPLETED
    let [completedRide] = completedRBStatusResult.rideList
    completedRide.status `shouldBe` BRide.COMPLETED
    return $ Just completedRide.id

  tRide' <- getBPPRideById tRide.id
  tRide'.traveledDistance.getHighPrecMeters `shouldSatisfy` equalsEps eps distance

  -- Leave feedback
  -- not yet implemented
  --  void . callBAP $ callAppFeedback 5 completedRideId

  void . callBPP $ setDriverOnline driverToken1 False
