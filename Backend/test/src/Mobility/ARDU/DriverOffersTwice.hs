{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.ARDU.DriverOffersTwice where

import Common
import EulerHS.Prelude
import HSpec
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Mobility.ARDU.APICalls as API
import Mobility.ARDU.Fixtures
import qualified Mobility.ARDU.Utils as Utils
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverQuote as QDrQuote
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl API.getDriverOfferBppBaseUrl
  describe "Driver offering quote twice immediately" $
    beforeAndAfter_
      ( do
          mapM_ Utils.resetDriver [arduDriver1, arduDriver2]
          Utils.resetCustomer appRegistrationToken
      )
      $ do
        it "Should throw an error: found active quotes" $
          driverOffersTwice clients

{-
  it "Should throw an error: driver on ride" $
    driverOffersOnRide clients
  -- it seems that this tests doesn't work because of old replica state
-}

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

  liftIO $ runARDUFlow "" $ Esq.runTransaction $ QDrQuote.setInactiveByRequestId @DynamicDriverAppFlow searchReqForDriver1.searchRequestId

driverOffersOnRide :: ClientEnvs -> IO ()
driverOffersOnRide clients = withBecknClients clients $ do
  let (origin, destination, searchReq) = karnatakaSearchReq

  Utils.setupDriver arduDriver1 origin
  scRes <- Utils.search'Confirm appRegistrationToken arduDriver1 searchReq
  let tRide = scRes.ride
      bBookingId = scRes.bapBookingId

  Utils.setupDriver arduDriver2 origin -- because no estimates will be returned if there are no available drivers
  quoteId2 <- Utils.search'Select appRegistrationToken2 searchReq

  Utils.startRide arduDriver1 origin tRide bBookingId

  (searchReqForDriver1 :| _) <- Utils.getNearbySearchRequestForDriver arduDriver1 quoteId2

  eithRes <- Utils.offerQuoteEither arduDriver1 defaultAllowedDriverFee searchReqForDriver1.searchRequestId
  shouldReturnErrorCode "error: driver is on ride" "DRIVER_ON_RIDE" eithRes

  liftIO $ runARDUFlow "" $ Esq.runTransaction $ QDrQuote.setInactiveByRequestId @DynamicDriverAppFlow searchReqForDriver1.searchRequestId

  Utils.endRide arduDriver1 destination tRide bBookingId
