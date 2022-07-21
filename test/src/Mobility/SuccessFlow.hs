module Mobility.SuccessFlow where

import qualified "beckn-transport" API.UI.Booking as TbeBookingAPI
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
import qualified "beckn-transport" Storage.Queries.Ride as TQRide
import "app-backend" Types.API.Quote (OfferRes (OnDemandCab))
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
  (quoteAPIEntity :| _) <- poll do
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

  -- Init ride from app backend
  initResult <-
    callBAP $
      appInitRide appRegistrationToken $ mkAppInitReq bapQuoteId
  let bapBookingId = initResult.bookingId

  void . poll $ do
    initRB <- getBAPBooking bapBookingId
    initRB.bppBookingId `shouldSatisfy` isJust
    return $ Just ()

  -- Confirm ride from app backend
  void . callBAP $
    appConfirmRide appRegistrationToken $ mkAppConfirmReq bapBookingId

  void . poll $
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

equalsEps :: Double -> Double -> Double -> Bool
equalsEps eps x y = abs (x - y) < eps

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $ do
    it "Testing API flow for successful booking and completion of ride" $
      successFlow clients

successFlow :: ClientEnvs -> IO ()
successFlow clients = withBecknClients clients $ do
  bBookingId <- doAnAppSearch

  tBooking <- poll $ do
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

  tRide <- poll $ do
    tRide <- getBPPRide tBooking.id
    tRide.status `shouldBe` TRide.NEW
    return $ Just tRide

  void . callBPP $
    rideStart driverToken1 tRide.id $
      buildStartRideReq tRide.otp

  void . poll $ do
    inprogressRBStatusResult <- callBAP (appBookingStatus bBookingId appRegistrationToken)
    inprogressRBStatusResult.rideList `shouldSatisfy` not . null
    inprogressRBStatusResult.status `shouldBe` AppRB.TRIP_ASSIGNED
    let [inprogressRide] = inprogressRBStatusResult.rideList
    inprogressRide.status `shouldBe` BRide.INPROGRESS
    return $ Just ()

  void . callBPP $ rideEnd driverToken1 tRide.id

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
