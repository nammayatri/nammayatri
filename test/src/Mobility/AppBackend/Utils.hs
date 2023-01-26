module Mobility.AppBackend.Utils where

import qualified Beckn.External.Maps as Maps
import Beckn.Prelude
import qualified "app-backend" Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Mobility.AppBackend.Fixtures as Fixtures
import qualified "app-backend" Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Utils (runAppFlow)

changeCachedMapsConfig :: Maps.MapsServiceConfig -> IO ()
changeCachedMapsConfig googleCfg = runAppFlow "change cached maps config" do
  let serviceConfig = DMSC.MapsServiceConfig googleCfg
  yatriServiceConfig <- DMSC.buildMerchantServiceConfig Fixtures.yatriMerchantId serviceConfig
  CQMSC.cacheMerchantServiceConfig yatriServiceConfig

clearCachedMapsConfig :: IO ()
clearCachedMapsConfig = runAppFlow "clear cached maps config" do
  CQMSC.clearCache Fixtures.yatriMerchantId (DMSC.MapsService Maps.Google)
