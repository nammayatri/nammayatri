module Mobility.SuccessFlow where

import Beckn.Types.Id
import qualified "app-backend" Domain.Types.Quote as BQuote
import qualified "app-backend" Domain.Types.Ride as BRide
import qualified "app-backend" Domain.Types.RideBooking as AppRB
import qualified "app-backend" Domain.Types.RideBooking as BRB
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import qualified "app-backend" Storage.Queries.Quote as BQQuote
import qualified "beckn-transport" Storage.Queries.Ride as TQRide
import qualified "beckn-transport" Storage.Queries.RideBooking as TQRB
import "app-backend" Types.API.Quote (OfferRes (OnDemandCab))
import qualified "beckn-transport" Types.API.RideBooking as RideBookingAPI
import qualified "beckn-transport" Types.Storage.Quote as TQuote
import qualified "beckn-transport" Types.Storage.Ride as TRide
import qualified "beckn-transport" Types.Storage.RideBooking as TRB
import Utils

doAnAppSearch :: HasCallStack => ClientsM (Id BQuote.Quote, Id BRB.RideBooking)
doAnAppSearch = do
  -- Driver sets online
  void . callBPP $ setDriverOnline driverToken1 True

  -- Do an App Search
  appSearchId <-
    callBAP $
      searchServices appRegistrationToken searchReq
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
  quoteAPIEntity.estimatedFare `shouldSatisfy` (> 100) -- should at least be more than 100

  -- Confirm ride from app backend
  confirmResult <-
    callBAP $
      appConfirmRide appRegistrationToken appSearchId bapQuoteId
  let bapRideBookingId = confirmResult.bookingId

  void . poll $
    callBAP (appRideBookingStatus bapRideBookingId appRegistrationToken)
      <&> (.status)
      >>= (`shouldBe` AppRB.CONFIRMED)
      <&> Just

  return (bapQuoteId, bapRideBookingId)

getBPPQuoteId ::
  Id BQuote.Quote ->
  ClientsM (Id TQuote.Quote)
getBPPQuoteId bapQuoteId = do
  mbBQuote <- liftIO $ runAppFlow "" $ BQQuote.findById bapQuoteId
  mbBQuote `shouldSatisfy` isJust
  let Just bQuote = mbBQuote
  return $ cast bQuote.bppQuoteId

getBPPRideBooking ::
  Id TQuote.Quote ->
  ClientsM TRB.RideBooking
getBPPRideBooking quoteId = do
  mbRideBooking <- liftIO $ runTransporterFlow "" $ TQRB.findByQuoteId quoteId
  mbRideBooking $> () `shouldSatisfy` isJust
  return $ fromJust mbRideBooking

getBPPRide ::
  Id TRB.RideBooking ->
  ClientsM TRide.Ride
getBPPRide rideBookingId = do
  mbRide <- liftIO $ runTransporterFlow "" $ TQRide.findActiveByRBId rideBookingId
  mbRide `shouldSatisfy` isJust
  return $ fromJust mbRide

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for successful booking and completion of ride" $ withBecknClients clients do
      (bapQuoteId, bRideBookingId) <- doAnAppSearch

      tRideBooking <- poll $ do
        tQuoteId <- getBPPQuoteId bapQuoteId
        trb <- getBPPRideBooking tQuoteId
        trb.status `shouldBe` TRB.CONFIRMED
        return $ Just trb

      rideInfo <-
        poll . callBPP $
          getNotificationInfo tRideBooking.id driverToken1
            <&> (.rideRequest)
      rideInfo.bookingId `shouldBe` tRideBooking.id

      -- Driver Accepts a ride
      void . callBPP $
        rideRespond tRideBooking.id driverToken1 $
          RideBookingAPI.SetDriverAcceptanceReq RideBookingAPI.ACCEPT

      tRide <- poll $ do
        tRide <- getBPPRide tRideBooking.id
        tRide.status `shouldBe` TRide.NEW
        return $ Just tRide

      void . callBPP $
        rideStart driverToken1 tRide.id $
          buildStartRideReq tRide.otp

      void . poll $ do
        inprogressRBStatusResult <- callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
        inprogressRBStatusResult.ride `shouldSatisfy` isJust
        inprogressRBStatusResult.status `shouldBe` AppRB.TRIP_ASSIGNED
        let Just inprogressRide = inprogressRBStatusResult.ride
        inprogressRide.status `shouldBe` BRide.INPROGRESS
        return $ Just ()

      void . callBPP $ rideEnd driverToken1 tRide.id

      completedRideId <- poll $ do
        completedRBStatusResult <- callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
        completedRBStatusResult.ride `shouldSatisfy` isJust
        completedRBStatusResult.status `shouldBe` AppRB.COMPLETED
        let Just completedRide = completedRBStatusResult.ride
        completedRide.status `shouldBe` BRide.COMPLETED
        return $ Just completedRide.id

      -- Leave feedback
      void . callBAP $ callAppFeedback 5 completedRideId

      void . callBPP $ setDriverOnline driverToken1 False
