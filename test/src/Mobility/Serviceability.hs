{-# LANGUAGE OverloadedLabels #-}

module Mobility.Serviceability where

import Common
import EulerHS.Prelude
import Mobility.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec
import Types.API.Serviceability
import qualified "app-backend" Types.Common as AppCommon
import Utils

ernakulamLocation :: AppCommon.GPS
ernakulamLocation = AppCommon.GPS "10.0739" "76.2733"

keralaLocation :: AppCommon.GPS
keralaLocation = AppCommon.GPS "10.5449" "76.4356"

karnatakaLocation :: AppCommon.GPS
karnatakaLocation = AppCommon.GPS "12.4725" "75.8328"

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

serviceableRide :: ClientEnv -> IO ()
serviceableRide appClientEnv =
  runClient appClientEnv (rideServiceability appRegistrationToken req)
    >>= verifyServiceability (RideServiceabilityRes True)
  where
    req = RideServiceabilityReq ernakulamLocation keralaLocation

nonServiceableRide :: ClientEnv -> IO ()
nonServiceableRide appClientEnv =
  runClient appClientEnv (rideServiceability appRegistrationToken req)
    >>= verifyServiceability (RideServiceabilityRes False)
  where
    req = RideServiceabilityReq keralaLocation karnatakaLocation

nonServiceableSearchRequest :: ClientEnv -> IO ()
nonServiceableSearchRequest appClientEnv = do
  let sreq = searchReq
  let updatedSearchReq =
        sreq
          & #origin . #gps .~ keralaLocation
          & #destination . #gps .~ karnatakaLocation

  result <- runClient appClientEnv (searchServices appRegistrationToken updatedSearchReq)
  verifyError 400 "PRODUCT_NOT_SERVICEABLE" result

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
    it "Serviceable ride" $ serviceableRide appClientEnv
    it "Non-serviceable ride" $ nonServiceableRide appClientEnv
    it "Non-serviceable search request" $ nonServiceableSearchRequest appClientEnv
