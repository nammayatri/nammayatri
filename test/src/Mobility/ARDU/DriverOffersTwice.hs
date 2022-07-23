module Mobility.ARDU.DriverOffersTwice where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id (Id)
import Beckn.Types.MapSearch
import Common
import qualified "app-backend" Domain.Types.Quote as AppQuote
import EulerHS.Prelude
import HSpec
import qualified Mobility.ARDU.APICalls as API
import Mobility.ARDU.Fixtures
import qualified Mobility.ARDU.Utils as Utils
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import Storage.Queries.DriverQuote (setInactiveByRequestId)
import qualified "app-backend" Types.API.Search as AppSearch
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl API.getDriverOfferBppBaseUrl
  describe "Driver offering quote twice immediately" $
    after_ (Utils.resetDriver driver1) $ do
      it "Should throw an error: found active quotes" $
        driverOffersTwice locationUpdatesRoute1 clients

customerSearch :: Text -> AppSearch.SearchReq -> ClientsM (Id AppQuote.Quote)
customerSearch appToken searchReq' = do
  appSearchId <- Utils.search appToken searchReq'
  (bapQuoteAPIEntity :| _) <- Utils.getOnSearchTaxiQuotesByTransporterName appToken appSearchId bapTransporterName
  let quoteId = bapQuoteAPIEntity.id
  Utils.select appToken quoteId
  pure quoteId

driverOffersTwice :: NonEmpty (NonEmpty LatLong) -> ClientEnvs -> IO ()
driverOffersTwice updates clients = withBecknClients clients $ do
  let (origin, _, searchReq') = Utils.searchReqFromUpdatesList updates

  Utils.setupDriver driver1 origin

  quoteId1 <- customerSearch appRegistrationToken searchReq'
  quoteId2 <- customerSearch appRegistrationToken2 searchReq'

  (searchReqForDriver1 :| _) <- Utils.getNearbySearchRequestForDriver driver1 quoteId1
  (searchReqForDriver2 :| _) <- Utils.getNearbySearchRequestForDriver driver1 quoteId2

  let driverFare = 30.5
  Utils.offerQuote_ driver1 driverFare searchReqForDriver1.searchRequestId

  eithRes <- Utils.offerQuoteEither driver1 driverFare searchReqForDriver2.searchRequestId
  shouldReturnErrorCode "error on active quotes found" "FOUND_ACTIVE_QUOTES" eithRes

  liftIO $ runARDUFlow "" $ Esq.runTransaction $ setInactiveByRequestId searchReqForDriver1.searchRequestId
