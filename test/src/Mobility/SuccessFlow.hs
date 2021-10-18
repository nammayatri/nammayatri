{-# LANGUAGE TypeApplications #-}

module Mobility.SuccessFlow where

import Beckn.Types.Id
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import qualified "beckn-transport" Types.API.Ride as RideAPI
import qualified "beckn-transport" Types.Storage.Case as TCase
import qualified "app-backend" Types.Storage.ProductInstance as BPI
import qualified "beckn-transport" Types.Storage.ProductInstance as TPI
import Utils

doAnAppSearch :: HasCallStack => ClientsM (Text, TPI.ProductInstance)
doAnAppSearch = do
  -- Driver sets online
  void . callBPP $ setDriverOnline driverToken True

  -- Do an App Search
  appCaseid <-
    liftIO UUID.nextRandom
      <&> UUID.toText
      >>= buildSearchReq
      >>= callBAP . searchServices appRegistrationToken
      <&> (.caseId)

  -- Do a Case Status request for getting product instance to confirm ride
  (productInstance :| _) <- poll do
    -- List all confirmed rides (type = RIDEORDER)
    callBAP (buildCaseStatusRes appCaseid)
      <&> (.productInstance)
      -- since all BPP can give quote for now we filter by orgId
      <&> filter (\p -> p.organizationId == Id bppTransporterOrgId)
      <&> nonEmpty
  let productInstanceId = getId productInstance.id

  case productInstance.estimatedFare of
    Just prodFare -> prodFare `shouldSatisfy` (> 100) -- should at least be more than 100
    Nothing -> expectationFailure "Estimated price is Nothing."

  -- Confirm ride from app backend
  void . callBAP $
    appConfirmRide appRegistrationToken $ buildAppConfirmReq appCaseid productInstanceId

  transporterOrderPi <- pollBPPForOrgOrderPi (cast productInstance.id) TPI.CONFIRMED TCase.RIDEORDER
  transporterOrderPi.udf4 `shouldSatisfy` isJust
  return (productInstanceId, transporterOrderPi)

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
      (productInstanceId, transporterOrderPi) <- doAnAppSearch
      let transporterOrderPiId = transporterOrderPi.id

      rideInfo <-
        poll $
          try @_ @SomeException (callBPP $ getNotificationInfo driverToken (Just $ cast transporterOrderPiId))
            <&> either (const Nothing) (.rideRequest)
      rideInfo.productInstanceId `shouldBe` transporterOrderPiId

      -- Driver Accepts a ride
      void . callBPP $
        rideRespond driverToken $
          RideAPI.SetDriverAcceptanceReq transporterOrderPiId RideAPI.ACCEPT

      tripAssignedPI <- pollBPPForOrgOrderPi (Id productInstanceId) TPI.TRIP_ASSIGNED TCase.RIDEORDER
      tripAssignedPI.status `shouldBe` TPI.TRIP_ASSIGNED

      void . callBPP $
        rideStart driverToken transporterOrderPiId $
          buildStartRideReq $
            fromJust transporterOrderPi.udf4

      bapOrderPi <-
        callBAP (buildListPIs BPI.INPROGRESS)
          <&> map (.productInstance)
          <&> filter (\pI -> pI.parentId == Just (Id productInstanceId))
          >>= expectSingletonList

      -- Update RIDEORDER PI to COMPLETED once driver ends his trip
      void . callBPP $ rideEnd driverToken transporterOrderPiId

      void $
        callBAP (buildListPIs BPI.COMPLETED)
          <&> map (.productInstance)
          <&> filter (\pI -> pI.id == bapOrderPi.id)
          >>= expectSingletonList

      -- Leave feedback
      void . callBAP $ callAppFeedback 5 bapOrderPi.id
