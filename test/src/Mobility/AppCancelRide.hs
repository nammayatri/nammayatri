module Mobility.AppCancelRide where

import Common (getAppBaseUrl)
import qualified "app-backend" Domain.Types.CancellationReason as AppCR
import qualified "app-backend" Domain.Types.RideBooking as AppRB
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import Mobility.SuccessFlow (doAnAppSearch)
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $ do
    it "Testing API flow for ride cancelled by App" . withBecknClients clients $ do
      (_, bRideBookingId) <- doAnAppSearch
      void . callBPP $ setDriverOnline driverToken1 True

      -- cancel request initiated by App
      void . callBAP $ cancelRide bRideBookingId appRegistrationToken (buildAppCancelReq AppCR.OnConfirm)

      void . poll $
        callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.CANCELLED)
          <&> Just

      void . callBPP $ setDriverOnline driverToken1 False
