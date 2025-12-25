{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking.Maps
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking.Maps
import qualified Domain.Action.RiderPlatform.RideBooking.Maps
import qualified "rider-app" Domain.Action.UI.Maps
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("maps" :> (PostMapsAutoComplete :<|> PostMapsGetPlaceDetails :<|> PostMapsGetPlaceName))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postMapsAutoComplete merchantId city :<|> postMapsGetPlaceDetails merchantId city :<|> postMapsGetPlaceName merchantId city

type PostMapsAutoComplete =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.MAPS / 'API.Types.Dashboard.RideBooking.Maps.POST_MAPS_AUTO_COMPLETE)
      :> API.Types.Dashboard.RideBooking.Maps.PostMapsAutoComplete
  )

type PostMapsGetPlaceDetails =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.MAPS / 'API.Types.Dashboard.RideBooking.Maps.POST_MAPS_GET_PLACE_DETAILS)
      :> API.Types.Dashboard.RideBooking.Maps.PostMapsGetPlaceDetails
  )

type PostMapsGetPlaceName =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.MAPS / 'API.Types.Dashboard.RideBooking.Maps.POST_MAPS_GET_PLACE_NAME)
      :> API.Types.Dashboard.RideBooking.Maps.PostMapsGetPlaceName
  )

postMapsAutoComplete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Maps.AutoCompleteReq -> Environment.FlowHandler Domain.Action.UI.Maps.AutoCompleteResp)
postMapsAutoComplete merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Maps.postMapsAutoComplete merchantShortId opCity apiTokenInfo customerId req

postMapsGetPlaceDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Maps.GetPlaceDetailsReq -> Environment.FlowHandler Domain.Action.UI.Maps.GetPlaceDetailsResp)
postMapsGetPlaceDetails merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Maps.postMapsGetPlaceDetails merchantShortId opCity apiTokenInfo customerId req

postMapsGetPlaceName :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Maps.GetPlaceNameReq -> Environment.FlowHandler Domain.Action.UI.Maps.GetPlaceNameResp)
postMapsGetPlaceName merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Maps.postMapsGetPlaceName merchantShortId opCity apiTokenInfo customerId req
