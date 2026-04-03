{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.ProviderPlatform.Fleet.LiveMap 
( API,
handler )
where
import EulerHS.Prelude hiding (sortOn)
import Servant
import Tools.Auth.Api
import Kernel.Utils.Common hiding (INFO)
import Storage.Beam.CommonInstances ()
import qualified API.Types.ProviderPlatform.Fleet.LiveMap
import qualified API.Types.ProviderPlatform.Fleet
import qualified Domain.Action.ProviderPlatform.Fleet.LiveMap
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Dashboard.Common
import qualified Kernel.External.Maps.Types
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment


type API = ("liveMap" :> GetLiveMapDrivers)
handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getLiveMapDrivers merchantId city
type GetLiveMapDrivers = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT)
                                  ('DSL)
                                  (('PROVIDER_FLEET) / ('API.Types.ProviderPlatform.Fleet.LIVE_MAP) / ('API.Types.ProviderPlatform.Fleet.LiveMap.GET_LIVE_MAP_DRIVERS)) :> API.Types.ProviderPlatform.Fleet.LiveMap.GetLiveMapDrivers)
getLiveMapDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe (Kernel.External.Maps.Types.LatLong) -> Environment.FlowHandler [API.Types.ProviderPlatform.Fleet.LiveMap.MapDriverInfoRes])
getLiveMapDrivers merchantShortId opCity apiTokenInfo radius fleetOwnerId driverIdForRadius point = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.LiveMap.getLiveMapDrivers merchantShortId opCity apiTokenInfo radius fleetOwnerId driverIdForRadius point



