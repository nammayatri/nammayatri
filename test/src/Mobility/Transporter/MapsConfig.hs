{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Mobility.Transporter.MapsConfig where

import "static-offer-driver-app" Domain.Types.Merchant
import "static-offer-driver-app" Domain.Types.Merchant.MerchantServiceConfig
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Id
import qualified Mobility.Transporter.Fixtures as Fixtures
import qualified "static-offer-driver-app" Storage.Queries.Merchant.MerchantServiceConfig as QMSC
import Test.Hspec
import "static-offer-driver-app" Tools.Maps
import Utils

spec :: Spec
spec = describe "Merchant maps configs" $ do
  it
    "Fetch google config"
    fetchGoogleConfig
  it
    "Fetch OSRM config"
    fetchOSRMConfig

-- We use direct calls to DB in this test because cache already changed for using mock-google
fetchConfig :: forall b. (Show b, Eq b) => Id Merchant -> Maps.MapsService -> (ServiceConfig -> b) -> b -> IO ()
fetchConfig merchantId serviceProvider getterFunc resultExpected = do
  Just cfg <-
    runTransporterFlow "" $
      QMSC.findByMerchantIdAndService merchantId (MapsService serviceProvider)
  getterFunc cfg.serviceConfig `shouldBe` resultExpected

fetchGoogleConfig :: IO ()
fetchGoogleConfig = do
  fetchConfig Fixtures.yatriPartnerMerchantId Google func (fromJust $ parseBaseUrl "https://maps.googleapis.com/maps/api/")
  where
    func (MapsServiceConfig (GoogleConfig cfg)) = cfg.googleMapsUrl

fetchOSRMConfig :: IO ()
fetchOSRMConfig = do
  fetchConfig Fixtures.yatriPartnerMerchantId OSRM func (fromJust $ parseBaseUrl "localhost:5000")
  where
    func (MapsServiceConfig (OSRMConfig cfg)) = cfg.osrmUrl
