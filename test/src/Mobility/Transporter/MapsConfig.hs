{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Mobility.Transporter.MapsConfig where

import qualified Beckn.External.Maps as Maps
import Beckn.Prelude
import Beckn.Types.Id
import "beckn-transport" Domain.Types.Merchant
import "beckn-transport" Domain.Types.Merchant.MerchantServiceConfig
import qualified "beckn-transport" Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC
import Test.Hspec
import "beckn-transport" Tools.Maps
import Utils

spec :: Spec
spec = describe "Merchant maps configs" $ do
  it
    "Fetch google config"
    fetchGoogleConfig
  it
    "Fetch OSRM config"
    fetchOSRMConfig

defaultMerchantId :: Id Merchant
defaultMerchantId = Id "7f7896dd-787e-4a0b-8675-e9e6fe93bb8f"

fetchConfig :: forall b. (Show b, Eq b) => Id Merchant -> Maps.MapsService -> (ServiceConfig -> b) -> b -> IO ()
fetchConfig merchantId serviceProvider getterFunc resultExpected = do
  Just cfg <-
    runTransporterFlow "" $
      QOMSC.findByMerchantIdAndService merchantId (MapsService serviceProvider)
  getterFunc cfg.serviceConfig `shouldBe` resultExpected

fetchGoogleConfig :: IO ()
fetchGoogleConfig =
  fetchConfig defaultMerchantId Google func (fromJust $ parseBaseUrl "https://maps.googleapis.com/maps/api/")
  where
    func (MapsServiceConfig (GoogleConfig cfg)) = cfg.googleMapsUrl

fetchOSRMConfig :: IO ()
fetchOSRMConfig = do
  fetchConfig defaultMerchantId OSRM func (fromJust $ parseBaseUrl "localhost:5000")
  where
    func (MapsServiceConfig (OSRMConfig cfg)) = cfg.osrmUrl
