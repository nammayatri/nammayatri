{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Mobility.Transporter.Serviceability where

import qualified "rider-app" API.UI.Search as AppBESearch
import "rider-app" API.UI.Serviceability
import Common
import EulerHS.Prelude
import Kernel.External.Maps.Types (LatLong (..))
import Mobility.AppBackend.APICalls
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec
import Utils

ernakulamLocation :: LatLong
ernakulamLocation = LatLong 10.0739 76.2733

keralaLocation :: LatLong
keralaLocation = LatLong 10.5449 76.4356

goaLocation :: LatLong
goaLocation = LatLong 15.404242 74.015735

verifyServiceability :: (Eq a, Show a) => a -> Either ClientError a -> IO ()
verifyServiceability expectedValue = \case
  Left _ -> expectationFailure "Expected success response"
  Right value -> value `shouldBe` expectedValue

serviceableOrigin :: ClientEnv -> IO ()
serviceableOrigin appClientEnv =
  runClient appClientEnv (originServiceability appRegistrationToken req)
    >>= verifyServiceability (ServiceabilityRes {serviceable = True, specialLocation = Nothing, geoJson = Nothing})
  where
    req = ServiceabilityReq ernakulamLocation

nonServiceableOrigin :: ClientEnv -> IO ()
nonServiceableOrigin appClientEnv =
  runClient appClientEnv (originServiceability appRegistrationToken req)
    >>= verifyServiceability (ServiceabilityRes {serviceable = False, specialLocation = Nothing, geoJson = Nothing})
  where
    req = ServiceabilityReq keralaLocation

serviceableDestination :: ClientEnv -> IO ()
serviceableDestination appClientEnv =
  runClient appClientEnv (destinationServiceability appRegistrationToken req)
    >>= verifyServiceability (ServiceabilityRes {serviceable = True, specialLocation = Nothing, geoJson = Nothing})
  where
    req = ServiceabilityReq keralaLocation

nonServiceableDestination :: ClientEnv -> IO ()
nonServiceableDestination appClientEnv =
  runClient appClientEnv (destinationServiceability appRegistrationToken req)
    >>= verifyServiceability (ServiceabilityRes {serviceable = False, specialLocation = Nothing, geoJson = Nothing})
  where
    req = ServiceabilityReq goaLocation

nonServiceableSearchRequest :: ClientEnv -> IO ()
nonServiceableSearchRequest appClientEnv = do
  let updatedSearchReq = case defaultSearchReq of
        AppBESearch.OneWaySearch req ->
          AppBESearch.OneWaySearch $
            req
              & #origin . #gps .~ keralaLocation
              & #destination . #gps .~ goaLocation
        AppBESearch.RentalSearch req ->
          AppBESearch.RentalSearch $
            req
              & #origin . #gps .~ keralaLocation
        AppBESearch.RecurringSearch req ->
          AppBESearch.RecurringSearch $
            req
              & #origin . #gps .~ keralaLocation
  result <- runClient appClientEnv (searchServices appRegistrationToken updatedSearchReq (Just defaultVersion) (Just defaultVersion))
  verifyError 400 "RIDE_NOT_SERVICEABLE" result

spec :: Spec
spec = do
  appManager <- runIO $ Client.newManager tlsManagerSettings
  let appBaseUrl = getAppBaseUrl
      appClientEnv = mkClientEnv appManager appBaseUrl

  describe "Testing Serviceability API" do
    it "Serviceable origin" $ serviceableOrigin appClientEnv
    it "Non-serviceable origin" $ nonServiceableOrigin appClientEnv
    it "Serviceable destination" $ serviceableDestination appClientEnv
    it "Non-serviceable destination" $ nonServiceableDestination appClientEnv
    it "Non-serviceable search request" $ nonServiceableSearchRequest appClientEnv
