module Mobility.ARDU.DriverAcceptsNonrelevantQuote where

import Beckn.Types.MapSearch
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
    after_ (mapM_ Utils.resetDriver [driver1, driver2]) $ do
      it "Should throw an irrelevant search request error" $
        driverOffersOnAnIrrelevantSearchRequest locationUpdatesRoute1 clients

driverOffersOnAnIrrelevantSearchRequest :: NonEmpty (NonEmpty LatLong) -> ClientEnvs -> IO ()
driverOffersOnAnIrrelevantSearchRequest updates clients = withBecknClients clients $ do
  let (origin, _, searchReq') = Utils.searchReqFromUpdatesList updates

  Utils.setupDriver driver1 origin
  Utils.setupDriver driver2 origin

  appSearchId <- Utils.search appRegistrationToken searchReq'

  (bapQuoteAPIEntity :| _) <- Utils.getOnSearchTaxiQuotesByTransporterName appRegistrationToken appSearchId bapTransporterName

  bapQuoteAPIEntity.estimatedFare `shouldSatisfy` (> 100) -- ?
  let quoteId = bapQuoteAPIEntity.id
  Utils.select appRegistrationToken quoteId

  -- first driver gets nearby requests
  (searchReqForDriver :| _) <- Utils.getNearbySearchRequestForDriver driver1 quoteId

  let firstDriverFare = 30.5
  Utils.offerQuote_ driver1 firstDriverFare searchReqForDriver.searchRequestId

  (selectedQuoteAPIEntity :| _) <- Utils.getSelectedQuotesByQuoteId appRegistrationToken quoteId
  let selectedQuoteId = selectedQuoteAPIEntity.id

  -- second driver gets nearby requests
  (searchReqForSecondDriver :| _) <- Utils.getNearbySearchRequestForDriver driver2 quoteId

  bRideBookingId <- Utils.initWithCheck appRegistrationToken selectedQuoteId

  void $ Utils.confirmWithCheck appRegistrationToken bRideBookingId
  --
  let secondDriverFare = 30.5
  eithRes <- Utils.offerQuoteEither driver2 secondDriverFare searchReqForSecondDriver.searchRequestId
  shouldReturnErrorCode "error on nonrelevant search request" "SEARCH_REQUEST_NOT_RELEVANT" eithRes
