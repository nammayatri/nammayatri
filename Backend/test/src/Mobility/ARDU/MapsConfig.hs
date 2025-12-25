{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Mobility.ARDU.MapsConfig where

import "dynamic-offer-driver-app" Domain.Types.MerchantServiceConfig
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Id
import qualified "dynamic-offer-driver-app" Storage.Queries.MerchantServiceConfig as QMSC
import Test.Hspec
import "dynamic-offer-driver-app" Tools.Maps
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
fetchConfig :: forall b. (Show b, Eq b) => Maps.MapsService -> (ServiceConfig -> Maybe b) -> b -> IO ()
fetchConfig serviceProvider getterFunc resultExpected = do
  Just cfg <-
    runARDUFlow "" $
      QMSC.findByServiceAndCity (MapsService serviceProvider) (Id "Merchnant-op-city")
  getterFunc cfg.serviceConfig `shouldBe` Just resultExpected

fetchGoogleConfig :: IO ()
fetchGoogleConfig = do
  fetchConfig Google func (fromJust $ parseBaseUrl "http://localhost:8019/")
  where
    func (MapsServiceConfig (GoogleConfig cfg)) = Just cfg.googleMapsUrl
    func _ = Nothing

fetchOSRMConfig :: IO ()
fetchOSRMConfig = do
  fetchConfig OSRM func (fromJust $ parseBaseUrl "localhost:5001")
  where
    func (MapsServiceConfig (OSRMConfig cfg)) = Just cfg.osrmUrl
    func _ = Nothing
