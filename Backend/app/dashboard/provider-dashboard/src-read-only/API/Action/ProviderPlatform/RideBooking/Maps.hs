{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.RideBooking.Maps
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Maps
import qualified Domain.Action.ProviderPlatform.RideBooking.Maps
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Maps
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "dynamic-offer-driver-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("maps" :> (PostMapsAutoComplete :<|> PostMapsGetPlaceName))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postMapsAutoComplete merchantId city :<|> postMapsGetPlaceName merchantId city

type PostMapsAutoComplete =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.MAPS / 'API.Types.Dashboard.RideBooking.Maps.POST_MAPS_AUTO_COMPLETE)
      :> API.Types.Dashboard.RideBooking.Maps.PostMapsAutoComplete
  )

type PostMapsGetPlaceName =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.MAPS / 'API.Types.Dashboard.RideBooking.Maps.POST_MAPS_GET_PLACE_NAME)
      :> API.Types.Dashboard.RideBooking.Maps.PostMapsGetPlaceName
  )

postMapsAutoComplete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Maps.AutoCompleteReq -> Environment.FlowHandler Domain.Action.UI.Maps.AutoCompleteResp)
postMapsAutoComplete merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Maps.postMapsAutoComplete merchantShortId opCity apiTokenInfo driverId req

postMapsGetPlaceName :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Maps.GetPlaceNameReq -> Environment.FlowHandler Domain.Action.UI.Maps.GetPlaceNameResp)
postMapsGetPlaceName merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Maps.postMapsGetPlaceName merchantShortId opCity apiTokenInfo driverId req
