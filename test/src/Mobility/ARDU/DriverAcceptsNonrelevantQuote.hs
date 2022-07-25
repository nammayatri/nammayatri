module Mobility.ARDU.DriverAcceptsNonrelevantQuote where

import Common
import EulerHS.Prelude
import HSpec
import qualified Mobility.ARDU.APICalls as API
import Mobility.ARDU.Fixtures
import qualified Mobility.ARDU.Utils as Utils
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl API.getDriverOfferBppBaseUrl
  describe "Driver offering on an irrelevant search request" $
    after_ (mapM_ Utils.resetDriver [arduDriver1, arduDriver2]) $ do
      it "Should throw an irrelevant search request error" $
        driverOffersOnAnIrrelevantSearchRequest clients

driverOffersOnAnIrrelevantSearchRequest :: ClientEnvs -> IO ()
driverOffersOnAnIrrelevantSearchRequest clients = withBecknClients clients $ do
  let (origin, _, searchReq') = route1SearchRequest

  Utils.setupDriver arduDriver1 origin
  Utils.setupDriver arduDriver2 origin

  appSearchId <- Utils.search appRegistrationToken searchReq'

  (bapQuoteAPIEntity :| _) <- Utils.getOnSearchTaxiQuotesByTransporterName appRegistrationToken appSearchId bapTransporterName

  bapQuoteAPIEntity.estimatedFare `shouldSatisfy` (> 100) -- ?
  let quoteId = bapQuoteAPIEntity.id
  Utils.select appRegistrationToken quoteId

  -- first driver gets nearby requests
  (searchReqForDriver :| _) <- Utils.getNearbySearchRequestForDriver arduDriver1 quoteId

  let firstDriverFare = 30.5
  Utils.offerQuote arduDriver1 firstDriverFare searchReqForDriver.searchRequestId

  (selectedQuoteAPIEntity :| _) <- Utils.getSelectedQuotesByQuoteId appRegistrationToken quoteId
  let selectedQuoteId = selectedQuoteAPIEntity.id

  -- second driver gets nearby requests
  (searchReqForSecondDriver :| _) <- Utils.getNearbySearchRequestForDriver arduDriver2 quoteId

  bRideBookingId <- Utils.initWithCheck appRegistrationToken selectedQuoteId

  void $ Utils.confirmWithCheck appRegistrationToken bRideBookingId
  --
  let secondDriverFare = 30.5
  eithRes <- Utils.offerQuoteEither arduDriver2 secondDriverFare searchReqForSecondDriver.searchRequestId
  shouldReturnErrorCode "error on nonrelevant search request" "SEARCH_REQUEST_NOT_RELEVANT" eithRes
