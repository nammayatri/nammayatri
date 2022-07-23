module Mobility.Transporter.SuccessFlow where

import qualified "beckn-transport" API.UI.Booking as TbeBookingAPI
import qualified "beckn-transport" API.UI.Ride as TbeRideAPI
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Common
import qualified "app-backend" Domain.Types.Booking as AppRB
import qualified "app-backend" Domain.Types.Booking as BRB
import qualified "beckn-transport" Domain.Types.Booking as TRB
import "beckn-transport" Domain.Types.Person as TPerson
import qualified "app-backend" Domain.Types.Ride as BRide
import qualified "beckn-transport" Domain.Types.Ride as TRide
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import qualified "app-backend" Storage.Queries.Booking as BQRB
import qualified "beckn-transport" Storage.Queries.Booking as TQRB
import Storage.Queries.DriverLocation
import Mobility.Fixtures.AppBackend
import Mobility.AppBackend.APICalls
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Transporter
import "beckn-transport" Storage.Queries.DriverLocation
import qualified "beckn-transport" Storage.Queries.Ride as TQRide
import "app-backend" Types.API.Quote (OfferRes (OnDemandCab))
import qualified "beckn-transport" Types.API.RideBooking as RideBookingAPI
import "app-backend" Types.API.Search
import Utils

doAnAppSearch :: HasCallStack => ClientsM (Id BRB.Booking)
doAnAppSearch = doAnAppSearchByReq searchReq

doAnAppSearchByReq :: HasCallStack => SearchReq -> ClientsM (Id BRB.Booking)
doAnAppSearchByReq searchReq' = do
  -- Driver sets online
  void . callBPP $ setDriverOnline driverToken1 True
  -- Moves driver to the pickup point
  let origin = case searchReq' of
        OneWaySearch req -> req.origin
        RentalSearch req -> req.origin
  preUpdate <- liftIO $ buildUpdateLocationRequest $ origin.gps :| []
  void . callBPP $
    updateLocation driverToken1 preUpdate

  -- Do an App Search
  appSearchId <-
    callBAP $
      searchServices appRegistrationToken searchReq'
        <&> (.searchId)

  -- Do a get quotes request for getting quotes to confirm ride
  (quoteAPIEntity :| _) <- pollDesc "get quotes" $ do
    -- List all confirmed rides (type = RIDEORDER)
    callBAP (getQuotes appSearchId appRegistrationToken)
      <&> (.quotes)
      -- since all BPP can give quote for now we filter by orgId
      <&> mapMaybe \case
        OnDemandCab p -> Just p
        _ -> Nothing
      <&> filter (\p -> p.agencyName == bapTransporterName)
      <&> nonEmpty
  let bapQuoteId = quoteAPIEntity.id

  -- check if calculated price is greater than 0
  quoteAPIEntity.estimatedFare `shouldSatisfy` (> 100)

  -- Confirm ride from app backend
  confirmRes <-
    callBAP $
      appConfirmRide appRegistrationToken bapQuoteId mkAppConfirmReq
  let bapBookingId = confirmRes.bookingId

  void . pollDesc "confirm ride" $
    callBAP (appBookingStatus bapBookingId appRegistrationToken)
      <&> (.status)
      >>= (`shouldBe` AppRB.CONFIRMED)
      <&> Just

  return bapBookingId

getBAPBooking ::
  Id BRB.Booking ->
  ClientsM BRB.Booking
getBAPBooking bapRBId = do
  mbBRB <- liftIO $ runAppFlow "" $ BQRB.findById bapRBId
  mbBRB `shouldSatisfy` isJust
  let Just bRB = mbBRB
  return bRB

getBPPBooking ::
  Id BRB.Booking ->
  ClientsM TRB.Booking
getBPPBooking bapRBId = do
  bRB <- getBAPBooking bapRBId
  bRB.bppBookingId `shouldSatisfy` isJust
  let Just bppBookingId = bRB.bppBookingId
  mbTRB <- liftIO $ runTransporterFlow "" $ TQRB.findById $ cast bppBookingId
  mbTRB $> () `shouldSatisfy` isJust
  let Just tRB = mbTRB
  return tRB

getBPPRide ::
  Id TRB.Booking ->
  ClientsM TRide.Ride
getBPPRide bookingId = do
  mbRide <- liftIO $ runTransporterFlow "" $ TQRide.findActiveByRBId bookingId
  mbRide `shouldSatisfy` isJust
  return $ fromJust mbRide

getBPPRideById ::
  Id TRide.Ride ->
  ClientsM TRide.Ride
getBPPRideById rideId = do
  mbRide <- liftIO $ runTransporterFlow "" $ TQRide.findById rideId
  mbRide `shouldSatisfy` isJust
  return $ fromJust mbRide

getBPPDriverLocation ::
  Id TPerson.Person ->
  ClientsM LatLong
getBPPDriverLocation driverId = do
  mbRes <- liftIO $ runTransporterFlow "" $ findById driverId
  mbRes `shouldSatisfy` isJust
  let res = fromJust mbRes
  pure $
    LatLong
      { lat = res.lat,
        lon = res.lon
      }

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $ do
    it "Testing API flow for successful booking and completion of ride" $
      successFlow clients

successFlow :: ClientEnvs -> IO ()
successFlow clients = withBecknClients clients $ do
  bBookingId <- doAnAppSearch

  tBooking <- pollDesc "booking confirmed" $ do
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
-- <<<<<<< HEAD:test/src/Mobility/SuccessFlow.hs
    rideRespond tBooking.id driverToken1 $
      TbeBookingAPI.SetDriverAcceptanceReq TbeBookingAPI.ACCEPT
-- =======
--    rideRespond tRideBooking.id driverToken1 $
--      RideBookingAPI.SetDriverAcceptanceReq RideBookingAPI.ACCEPT
-- >>>>>>> slight refactoring of tests structure, readable local testing data for ARDU flow, healthcheck test for ARDU:test/src/Mobility/Transporter/SuccessFlow.hs

  tRide <- poll $ do
    tRide <- getBPPRide tBooking.id
    tRide.status `shouldBe` TRide.NEW
    return $ Just tRide

  void . callBPP $
    rideStart driverToken1 tRide.id $
      buildStartRideReq tRide.otp searchReqOrigin

  void . poll $ do
    inprogressRBStatusResult <- callBAP (appBookingStatus bBookingId appRegistrationToken)
    inprogressRBStatusResult.rideList `shouldSatisfy` not . null
    inprogressRBStatusResult.status `shouldBe` AppRB.TRIP_ASSIGNED
    let [inprogressRide] = inprogressRBStatusResult.rideList
    inprogressRide.status `shouldBe` BRide.INPROGRESS
    return $ Just ()

  void . callBPP $ rideEnd driverToken1 tRide.id $ TbeRideAPI.EndRideReq searchReqDestination

  completedRideId <- poll $ do
    completedRBStatusResult <- callBAP (appBookingStatus bBookingId appRegistrationToken)
    completedRBStatusResult.rideList `shouldSatisfy` not . null
    completedRBStatusResult.status `shouldBe` AppRB.COMPLETED
    let [completedRide] = completedRBStatusResult.rideList
    completedRide.status `shouldBe` BRide.COMPLETED
    return $ Just completedRide.id

  -- Leave feedback
  void . callBAP $ callAppFeedback 5 completedRideId

  void . callBPP $ setDriverOnline driverToken1 False
