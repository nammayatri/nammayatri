module Mobility.ARDU.CancelFlow where

import Common (getAppBaseUrl)
import EulerHS.Prelude
import HSpec
import Mobility.ARDU.APICalls (getDriverOfferBppBaseUrl)
import Mobility.ARDU.Fixtures (arduDriver1)
import Mobility.ARDU.Utils as Utils
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getDriverOfferBppBaseUrl
  describe "Testing App and Transporter APIs"
    . after_ (Utils.resetDriver arduDriver1)
    $ do
      it "Testing API flow for ride cancelled by Driver" $
        withBecknClients clients cancelFlow
      it "Testing API flow for ride cancelled by app" $
        withBecknClients clients appCancelFlow

cancelFlow :: ClientsM ()
cancelFlow = do
  let (origin, _, searchReq') = route1SearchRequest
  Utils.setupDriver arduDriver1 origin
  scRes <- search'Confirm appRegistrationToken arduDriver1 searchReq'
  cancelRideByDriver arduDriver1 scRes.bapBookingId scRes.ride

appCancelFlow :: ClientsM ()
appCancelFlow = do
  let (origin, _, searchReq') = route1SearchRequest
  Utils.setupDriver arduDriver1 origin
  scRes <- search'Confirm appRegistrationToken arduDriver1 searchReq'
  cancelRideByApp appRegistrationToken arduDriver1 scRes.bapBookingId
