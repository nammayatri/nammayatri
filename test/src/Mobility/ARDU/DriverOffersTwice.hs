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
import Storage.Queries.DriverQuote (setInactiveByRequestId)
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl API.getDriverOfferBppBaseUrl
  describe "Driver offering quote twice immediately" $
    after_ (Utils.resetDriver arduDriver1) $ do
      it "Should throw an error: found active quotes" $
        driverOffersTwice clients

driverOffersTwice :: ClientEnvs -> IO ()
driverOffersTwice clients = withBecknClients clients $ do
  let (origin, _, searchReq') = route1SearchRequest

  Utils.setupDriver arduDriver1 origin

  quoteId1 <- Utils.search'Select appRegistrationToken searchReq'
  quoteId2 <- Utils.search'Select appRegistrationToken2 searchReq'

  (searchReqForDriver1 :| _) <- Utils.getNearbySearchRequestForDriver arduDriver1 quoteId1
  (searchReqForDriver2 :| _) <- Utils.getNearbySearchRequestForDriver arduDriver1 quoteId2

  Utils.offerQuote arduDriver1 defaultAllowedDriverFee searchReqForDriver1.searchRequestId

  eithRes <- Utils.offerQuoteEither arduDriver1 defaultAllowedDriverFee searchReqForDriver2.searchRequestId
  shouldReturnErrorCode "error on active quotes found" "FOUND_ACTIVE_QUOTES" eithRes

  liftIO $ runARDUFlow "" $ Esq.runTransaction $ setInactiveByRequestId searchReqForDriver1.searchRequestId
