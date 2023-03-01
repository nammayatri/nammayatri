{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.ARDU.DriverAcceptsNonrelevantQuote where

import Common
import qualified "rider-app" Domain.Action.UI.Select as DSelect
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
    . beforeAndAfter_
      ( do
          mapM_ Utils.resetDriver [arduDriver1, arduDriver2]
          Utils.resetCustomer appRegistrationToken
      )
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
  Utils.select appRegistrationToken quoteId DSelect.DEstimateSelectReq {customerExtraFee = Just 15, autoAssignEnabled = False}

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
