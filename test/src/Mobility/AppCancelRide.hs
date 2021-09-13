module Mobility.AppCancelRide where

import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import Mobility.SuccessFlow (doAnAppSearch)
import qualified "app-backend" Types.Storage.CancellationReason as AppCR
import qualified "app-backend" Types.Storage.RideBooking as AppRB
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $ do
    it "Testing API flow for ride cancelled by App" . withBecknClients clients $ do
      (_, bRideBookingId) <- doAnAppSearch

      -- cancel request initiated by App
      void . callBAP $ cancelRide bRideBookingId appRegistrationToken (buildAppCancelReq AppCR.OnConfirm)

      void . poll $
        callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.CANCELLED)
          <&> Just
