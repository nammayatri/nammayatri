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
  describe "Driver offering on an irrelevant search request"
    . after_ (mapM_ Utils.resetDriver [arduDriver1, arduDriver2])
    $ it "Should throw an irrelevant search request error" $
      driverOffersOnAnIrrelevantSearchRequest clients

driverOffersOnAnIrrelevantSearchRequest :: ClientEnvs -> IO ()
driverOffersOnAnIrrelevantSearchRequest clients = withBecknClients clients $ do
  let (origin, _, searchReq') = karnatakaSearchReq

  Utils.setupDriver arduDriver1 origin
  Utils.setupDriver arduDriver2 origin

  appSearchId <- Utils.search appRegistrationToken searchReq'

  (bapQuoteAPIEntity :| _) <- Utils.getOnSearchTaxiEstimatesByTransporterName appRegistrationToken appSearchId bapTransporterName

  bapQuoteAPIEntity.estimatedFare `shouldSatisfy` (> 100) -- ?
  let quoteId = bapQuoteAPIEntity.id
  Utils.select appRegistrationToken quoteId

  -- first driver gets nearby requests
  (searchReqForDriver :| _) <- Utils.getNearbySearchRequestForDriver arduDriver1 quoteId

  Utils.offerQuote arduDriver1 defaultAllowedDriverFee searchReqForDriver.searchRequestId

  (selectedQuoteAPIEntity :| _) <- Utils.getQuotesByEstimateId appRegistrationToken quoteId
  let selectedQuoteId = selectedQuoteAPIEntity.id

  -- second driver gets nearby requests
  (searchReqForSecondDriver :| _) <- Utils.getNearbySearchRequestForDriver arduDriver2 quoteId

  (bapBookingId, _, bppRide) <- Utils.confirmWithCheck appRegistrationToken selectedQuoteId
  --
  eithRes <- Utils.offerQuoteEither arduDriver2 defaultAllowedDriverFee searchReqForSecondDriver.searchRequestId
  shouldReturnErrorCode "error on nonrelevant search request" "NO_SEARCH_REQUEST_FOR_DRIVER" eithRes

  Utils.cancelRideByDriver arduDriver1 bapBookingId bppRide
