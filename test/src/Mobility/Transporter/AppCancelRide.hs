module Mobility.Transporter.AppCancelRide where

import Common (getAppBaseUrl)
import qualified "app-backend" Domain.Types.Booking as AppRB
import qualified "app-backend" Domain.Types.CancellationReason as AppCR
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures.AppBackend
import Mobility.Fixtures.Transporter
import Mobility.Transporter.SuccessFlow (doAnAppSearch)
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $ do
    it "Testing API flow for ride cancelled by App" . withBecknClients clients $ do
      bBookingId <- doAnAppSearch
      void . callBPP $ setDriverOnline driverToken1 True

      -- cancel request initiated by App
      void . callBAP $ cancelRide bBookingId appRegistrationToken (mkAppCancelReq AppCR.OnConfirm)

      void . poll $
        callBAP (appBookingStatus bBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.CANCELLED)
          <&> Just

      void . callBPP $ setDriverOnline driverToken1 False
