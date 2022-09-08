{-# LANGUAGE OverloadedLabels #-}

module Mobility.Transporter.Serviceability where

import qualified "app-backend" API.UI.Search as AppBESearch
import Beckn.Types.MapSearch (LatLong (..))
import Common
import EulerHS.Prelude
import Mobility.AppBackend.APICalls
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec
import Types.API.Serviceability
import Utils

ernakulamLocation :: LatLong
ernakulamLocation = LatLong 10.0739 76.2733

keralaLocation :: LatLong
keralaLocation = LatLong 10.5449 76.4356

karnatakaLocation :: LatLong
karnatakaLocation = LatLong 12.4725 75.8328

verifyServiceability :: (Eq a, Show a) => a -> Either ClientError a -> IO ()
verifyServiceability expectedValue = \case
  Left _ -> expectationFailure "Expected success response"
  Right value -> value `shouldBe` expectedValue

serviceableOrigin :: ClientEnv -> IO ()
serviceableOrigin appClientEnv =
  runClient appClientEnv (originServiceability appRegistrationToken req)
    >>= verifyServiceability (ServiceabilityRes True)
  where
    req = ServiceabilityReq ernakulamLocation

nonServiceableOrigin :: ClientEnv -> IO ()
nonServiceableOrigin appClientEnv =
  runClient appClientEnv (originServiceability appRegistrationToken req)
    >>= verifyServiceability (ServiceabilityRes False)
  where
    req = ServiceabilityReq keralaLocation

serviceableDestination :: ClientEnv -> IO ()
serviceableDestination appClientEnv =
  runClient appClientEnv (destinationServiceability appRegistrationToken req)
    >>= verifyServiceability (ServiceabilityRes True)
  where
    req = ServiceabilityReq keralaLocation

nonServiceableDestination :: ClientEnv -> IO ()
nonServiceableDestination appClientEnv =
  runClient appClientEnv (destinationServiceability appRegistrationToken req)
    >>= verifyServiceability (ServiceabilityRes False)
  where
    req = ServiceabilityReq karnatakaLocation

nonServiceableSearchRequest :: ClientEnv -> IO ()
nonServiceableSearchRequest appClientEnv = do
  let updatedSearchReq = case defaultSearchReq of
        AppBESearch.OneWaySearch req ->
          AppBESearch.OneWaySearch $
            req
              & #origin . #gps .~ keralaLocation
              & #destination . #gps .~ karnatakaLocation
        AppBESearch.RentalSearch req ->
          AppBESearch.RentalSearch $
            req
              & #origin . #gps .~ keralaLocation
  result <- runClient appClientEnv (searchServices appRegistrationToken updatedSearchReq)
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
