module Mobility.SuccessFlow where

import Beckn.Types.Id
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import qualified "beckn-transport" Types.API.Ride as RideAPI
import qualified "beckn-transport" Types.Storage.Case as TCase
import qualified "app-backend" Types.Storage.ProductInstance as BPI
import qualified "beckn-transport" Types.Storage.ProductInstance as TPI
import qualified Types.Storage.Ride as AppRide
import qualified "app-backend" Types.Storage.RideBooking as AppRB
import Utils

doAnAppSearch :: HasCallStack => ClientsM (Id BPI.ProductInstance, Id BPI.ProductInstance, TPI.ProductInstance)
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

  transporterOrderPi <- pollBPPForOrgOrderPi (cast bQuoteId) TPI.CONFIRMED TCase.RIDEORDER
  transporterOrderPi.udf4 `shouldSatisfy` isJust

  return (bQuoteId, bRideBookingId, transporterOrderPi)

pollBPPForOrgOrderPi ::
  Id TPI.ProductInstance ->
  TPI.ProductInstanceStatus ->
  TCase.CaseType ->
  ClientsM TPI.ProductInstance
pollBPPForOrgOrderPi searchPiId status type_ =
  expectSingletonNE <=< poll $ do
    -- List all confirmed rides (type = RIDEORDER)
    callBPP (buildOrgRideReq status type_)
      <&> map (.productInstance)
      <&> filter (\pI -> pI.parentId == Just searchPiId) -- Filter order productInstance
      <&> nonEmpty

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for successful booking and completion of ride" $ withBecknClients clients do
      (productInstanceId, bRideBookingId, transporterOrderPi) <- doAnAppSearch
      let transporterOrderPiId = transporterOrderPi.id

      rideInfo <-
        poll . callBPP $
          getNotificationInfo driverToken (Just $ cast transporterOrderPiId)
            <&> (.rideRequest)
      rideInfo.productInstanceId `shouldBe` transporterOrderPiId

      -- Driver Accepts a ride
      void . callBPP $
        rideRespond driverToken $
          RideAPI.SetDriverAcceptanceReq transporterOrderPiId RideAPI.ACCEPT

      tripAssignedPI <- pollBPPForOrgOrderPi (cast productInstanceId) TPI.TRIP_ASSIGNED TCase.RIDEORDER
      tripAssignedPI.status `shouldBe` TPI.TRIP_ASSIGNED

      void . callBPP $
        rideStart driverToken transporterOrderPiId $
          buildStartRideReq $
            fromJust transporterOrderPi.udf4

      void . poll $ do 
        inprogressRBStatusResult <- callBAP (rideBookingStatus bRideBookingId appRegistrationToken)
        inprogressRBStatusResult.ride `shouldSatisfy` isJust
        inprogressRBStatusResult.status `shouldBe` AppRB.TRIP_ASSIGNED
        let Just inprogressRide = inprogressRBStatusResult.ride
        inprogressRide.status `shouldBe` AppRide.INPROGRESS
        return $ Just ()

      void . callBPP $ rideEnd driverToken transporterOrderPiId

      completedRideId <- poll $ do
        completedRBStatusResult <- callBAP (rideBookingStatus bRideBookingId appRegistrationToken)
        completedRBStatusResult.ride `shouldSatisfy` isJust
        completedRBStatusResult.status `shouldBe` AppRB.COMPLETED
        let Just completedRide = completedRBStatusResult.ride
        completedRide.status `shouldBe` AppRide.COMPLETED
        return $ Just completedRide.id

      -- Leave feedback
      void . callBAP $ callAppFeedback 5 completedRideId
