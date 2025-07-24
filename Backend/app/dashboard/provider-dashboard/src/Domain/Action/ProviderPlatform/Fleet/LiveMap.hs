module Domain.Action.ProviderPlatform.Fleet.LiveMap (getLiveMapDrivers) where

import qualified API.Client.ProviderPlatform.Fleet
import qualified API.Types.ProviderPlatform.Fleet.LiveMap
import qualified "dashboard-helper-api" Dashboard.Common
import Domain.Action.ProviderPlatform.Fleet.Driver (getMbFleetOwnerAndRequestorIdMerchantBased)
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getLiveMapDrivers ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Common.Meters ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) ->
  Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong ->
  Environment.Flow [API.Types.ProviderPlatform.Fleet.LiveMap.MapDriverInfoRes]
getLiveMapDrivers merchantShortId opCity apiTokenInfo radius mbFleetOwnerId mbDriverIdForRadius mbPoint = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (mbFleetOwnerId', requestorId) <- getMbFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
  API.Client.ProviderPlatform.Fleet.callFleetAPI checkedMerchantId opCity (.liveMapDSL.getLiveMapDrivers) radius requestorId mbFleetOwnerId' mbDriverIdForRadius mbPoint
