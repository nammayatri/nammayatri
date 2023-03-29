{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Mobility.Transporter.MapsConfig where

import "static-offer-driver-app" Domain.Types.Merchant
import "static-offer-driver-app" Domain.Types.Merchant.MerchantServiceConfig
import Kernel.Prelude
import qualified Kernel.Types.CommonImport as Maps
import Kernel.Types.Id
import qualified Mobility.Transporter.Fixtures as Fixtures
import qualified "static-offer-driver-app" Storage.Queries.Merchant.MerchantServiceConfig as QMSC
import Test.Hspec
import "static-offer-driver-app" Tools.Maps (MapsServiceConfig (GoogleConfig, OSRMConfig))
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
  fetchConfig Fixtures.yatriPartnerMerchantId Maps.Google func (fromJust $ parseBaseUrl "http://localhost:8019/")
  where
    func (MapsServiceConfig (GoogleConfig cfg)) = cfg.googleMapsUrl

fetchOSRMConfig :: IO ()
fetchOSRMConfig = do
  fetchConfig Fixtures.yatriPartnerMerchantId Maps.OSRM func (fromJust $ parseBaseUrl "localhost:5000")
  where
    func (MapsServiceConfig (OSRMConfig cfg)) = cfg.osrmUrl
