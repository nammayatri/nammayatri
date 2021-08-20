module Mobility.SuccessFlow where

import Beckn.Types.Id
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import qualified "beckn-transport" Types.API.RideBooking as RideBookingAPI
import qualified "app-backend" Types.Storage.Quote as BQuote
import qualified "beckn-transport" Types.Storage.Quote as TQuote
import qualified "app-backend" Types.Storage.RideBooking as AppRB
import Utils
import qualified "beckn-transport" Storage.Queries.RideBooking as TQRB
import qualified "beckn-transport" Types.Storage.RideBooking as TRB
import qualified "app-backend" Types.Storage.RideBooking as BRB
import qualified "app-backend" Types.Storage.OldRide as BRide

doAnAppSearch :: HasCallStack => ClientsM (Id BQuote.Quote, Id BRB.RideBooking, TRB.RideBooking)
doAnAppSearch = do
  -- Driver sets online
  void . callBPP $ setDriverOnline driverToken True

  -- Do an App Search
  appSearchId <-
    callBAP $
      searchServices appRegistrationToken searchReq
        <&> (.searchId)

  -- Do a Case Status request for getting product instance to confirm ride
  quoteAPIEntity <- expectSingletonNE <=< poll $ do
    -- List all confirmed rides (type = RIDEORDER)
    callBAP (getQuotes appSearchId appRegistrationToken)
      <&> (.quotes)
      -- since all BPP can give quote for now we filter by orgId
      <&> filter (\p -> p.agencyName == bapTransporterName)
      <&> nonEmpty
  let bQuoteId = quoteAPIEntity.id

  -- check if calculated price is greater than 0
  quoteAPIEntity.estimatedPrice `shouldSatisfy` (> 100) -- should at least be more than 100

  -- Confirm ride from app backend
  confirmResult <-
    callBAP $
      appConfirmRide appRegistrationToken appSearchId bQuoteId
  let bRideBookingId = confirmResult.bookingId

  tRideBooking <- getBPPRideBooking (cast bQuoteId)
  tRideBooking.udf4 `shouldSatisfy` isJust

  return (bQuoteId, bRideBookingId, tRideBooking)

getBPPRideBooking ::
  Id TQuote.Quote ->
  ClientsM TRB.RideBooking
getBPPRideBooking quoteId = do
  mbRideBooking <- liftIO $ runTransporterFlow "" $ TQRB.findByQuoteId (cast quoteId)
  mbRideBooking `shouldSatisfy` isJust
  return $ fromJust mbRideBooking

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for successful booking and completion of ride" $ withBecknClients clients do
      (quoteId, bRideBookingId, tRideBooking) <- doAnAppSearch

      rideInfo <-
        poll . callBPP $
          getNotificationInfo (cast tRideBooking.id) driverToken
            <&> (.rideRequest)
      rideInfo.bookingId `shouldBe` tRideBooking.id

      -- Driver Accepts a ride
      void . callBPP $
        rideRespond tRideBooking.id driverToken $
          RideBookingAPI.SetDriverAcceptanceReq RideBookingAPI.ACCEPT

      void . poll $
        getBPPRideBooking (cast quoteId)
          <&> (.status)
          >>= (`shouldBe` TRB.TRIP_ASSIGNED)
          <&> Just

      void . callBPP $
        rideStart driverToken tRideBooking.id $
          buildStartRideReq $
            fromJust tRideBooking.udf4

      void . poll $ do
        inprogressRBStatusResult <- callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
        inprogressRBStatusResult.ride `shouldSatisfy` isJust
        inprogressRBStatusResult.status `shouldBe` AppRB.TRIP_ASSIGNED
        let Just inprogressRide = inprogressRBStatusResult.ride
        inprogressRide.status `shouldBe` BRide.INPROGRESS
        return $ Just ()

      void . callBPP $ rideEnd driverToken tRideBooking.id

      completedRideId <- poll $ do
        completedRBStatusResult <- callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
        completedRBStatusResult.ride `shouldSatisfy` isJust
        completedRBStatusResult.status `shouldBe` AppRB.COMPLETED
        let Just completedRide = completedRBStatusResult.ride
        completedRide.status `shouldBe` BRide.COMPLETED
        return $ Just completedRide.id

      -- Leave feedback
      void . callBAP $ callAppFeedback 5 completedRideId