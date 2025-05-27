{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Fleet.LiveMap (getLiveMapDrivers) where

import qualified API.Client.ProviderPlatform.Fleet
import qualified API.Types.ProviderPlatform.Fleet.LiveMap
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getLiveMapDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow [API.Types.ProviderPlatform.Fleet.LiveMap.MapDriverInfoRes])
getLiveMapDrivers merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Fleet.callFleetAPI checkedMerchantId opCity (.liveMapDSL.getLiveMapDrivers)
