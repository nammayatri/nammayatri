module Mobility.Transporter.AppCancelRide where

import Common (getAppBaseUrl)
import EulerHS.Prelude
import HSpec
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import qualified Mobility.Transporter.APICalls as API
import Mobility.Transporter.Fixtures
import qualified Mobility.Transporter.Utils as Utils
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl API.getTransporterBaseUrl
  beforeAndAfter_
    ( do
        Utils.resetDriver transporterDriver1
        Utils.resetCustomer appRegistrationToken
    )
    $ it "Testing API flow for ride cancelled by App" . withBecknClients clients $ do
      let (origin, _destination, searchReq_) = route1SearchRequest
      Utils.setupDriver transporterDriver1 origin

      Utils.search'Confirm appRegistrationToken searchReq_ \scRes -> do
        let bBookingId = scRes.bapBookingId

        -- cancel request initiated by App
        Utils.cancelRideByApp appRegistrationToken bBookingId
