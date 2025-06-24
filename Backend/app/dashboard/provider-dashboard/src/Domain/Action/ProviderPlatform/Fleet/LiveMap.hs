module Domain.Action.ProviderPlatform.Fleet.LiveMap (getLiveMapDrivers) where

import qualified API.Client.ProviderPlatform.Fleet
import qualified API.Types.ProviderPlatform.Fleet.LiveMap
import Domain.Action.ProviderPlatform.Fleet.Driver (getMbFleetOwnerAndRequestorIdMerchantBased)
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getLiveMapDrivers ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Environment.Flow [API.Types.ProviderPlatform.Fleet.LiveMap.MapDriverInfoRes]
getLiveMapDrivers merchantShortId opCity apiTokenInfo mbFleetOwnerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (mbFleetOwnerId', requestorId) <- getMbFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
  API.Client.ProviderPlatform.Fleet.callFleetAPI checkedMerchantId opCity (.liveMapDSL.getLiveMapDrivers) requestorId mbFleetOwnerId'
