module Mobility.ARDU.DriverOffersTwice where

import qualified Beckn.Storage.Esqueleto as Esq
import Common
import EulerHS.Prelude
import HSpec
import qualified Mobility.ARDU.APICalls as API
import Mobility.ARDU.Fixtures
import qualified Mobility.ARDU.Utils as Utils
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import qualified "driver-offer-bpp" Storage.Queries.DriverQuote as QDrQuote
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl API.getDriverOfferBppBaseUrl
  describe "Driver offering quote twice immediately" $
    after_ (mapM_ Utils.resetDriver [arduDriver1, arduDriver2]) $ do
      it "Should throw an error: found active quotes" $
        driverOffersTwice clients
      it "Should throw an error: driver on ride" $
        driverOffersOnRide clients

driverOffersTwice :: ClientEnvs -> IO ()
driverOffersTwice clients = withBecknClients clients $ do
  let (origin, _, searchReq') = karnatakaSearchReq

  Utils.setupDriver arduDriver1 origin

  quoteId1 <- Utils.search'Select appRegistrationToken searchReq'
  quoteId2 <- Utils.search'Select appRegistrationToken2 searchReq'

  (searchReqForDriver1 :| _) <- Utils.getNearbySearchRequestForDriver arduDriver1 quoteId1
  (searchReqForDriver2 :| _) <- Utils.getNearbySearchRequestForDriver arduDriver1 quoteId2

  Utils.offerQuote arduDriver1 defaultAllowedDriverFee searchReqForDriver1.searchRequestId

  eithRes <- Utils.offerQuoteEither arduDriver1 defaultAllowedDriverFee searchReqForDriver2.searchRequestId
  shouldReturnErrorCode "error on active quotes found" "FOUND_ACTIVE_QUOTES" eithRes

  liftIO $ runARDUFlow "" $ Esq.runTransaction $ QDrQuote.setInactiveByRequestId searchReqForDriver1.searchRequestId

driverOffersOnRide :: ClientEnvs -> IO ()
driverOffersOnRide clients = withBecknClients clients $ do
  let (origin, destination, searchReq) = karnatakaSearchReq

  Utils.setupDriver arduDriver1 origin
  scRes <- Utils.search'Confirm appRegistrationToken arduDriver1 searchReq
  let tRide = scRes.ride
      bBookingId = scRes.bapBookingId

  Utils.startRide arduDriver1 origin tRide bBookingId

  Utils.setupDriver arduDriver2 origin -- because no estimates will be returned if there are no available drivers
  quoteId2 <- Utils.search'Select appRegistrationToken2 searchReq

  (searchReqForDriver1 :| _) <- Utils.getNearbySearchRequestForDriver arduDriver1 quoteId2

  eithRes <- Utils.offerQuoteEither arduDriver1 defaultAllowedDriverFee searchReqForDriver1.searchRequestId
  shouldReturnErrorCode "error: driver is on ride" "DRIVER_ON_RIDE" eithRes

  liftIO $ runARDUFlow "" $ Esq.runTransaction $ QDrQuote.setInactiveByRequestId searchReqForDriver1.searchRequestId

  Utils.endRide arduDriver1 destination tRide bBookingId
