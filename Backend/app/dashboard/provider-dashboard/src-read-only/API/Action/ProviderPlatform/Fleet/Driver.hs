{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Fleet.Driver
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet
import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified Dashboard.Common
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import qualified Domain.Action.ProviderPlatform.Fleet.Driver
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("driver" :> (PostDriverFleetAddVehicles :<|> PostDriverFleetAddVehicle :<|> GetDriverFleetGetDriverRequests :<|> PostDriverFleetRespondDriverRequest :<|> PostDriverFleetAddRCWithoutDriver :<|> GetDriverFleetGetAllVehicle :<|> GetDriverFleetGetAllDriver :<|> PostDriverFleetUnlink :<|> PostDriverFleetRemoveVehicle :<|> PostDriverFleetRemoveDriver :<|> GetDriverFleetTotalEarning :<|> GetDriverFleetVehicleEarning :<|> GetDriverFleetDriverEarning :<|> GetDriverFleetDriverVehicleAssociation :<|> GetDriverFleetDriverAssociation :<|> GetDriverFleetVehicleAssociation :<|> PostDriverFleetVehicleDriverRcStatus :<|> PostDriverUpdateFleetOwnerInfo :<|> GetDriverFleetOwnerInfo :<|> PostDriverFleetSendJoiningOtp :<|> PostDriverFleetVerifyJoiningOtp :<|> GetDriverFleetRoutes :<|> GetDriverFleetPossibleRoutes :<|> PostDriverFleetTripPlanner :<|> GetDriverFleetTripTransactions :<|> PostDriverFleetAddDrivers :<|> PostDriverFleetAddDriverBusRouteMapping :<|> PostDriverFleetLinkRCWithDriver :<|> PostDriverDashboardFleetWmbTripEnd :<|> GetDriverFleetWmbRouteDetails :<|> PostDriverDashboardFleetTrackDriver))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDriverFleetAddVehicles merchantId city :<|> postDriverFleetAddVehicle merchantId city :<|> getDriverFleetGetDriverRequests merchantId city :<|> postDriverFleetRespondDriverRequest merchantId city :<|> postDriverFleetAddRCWithoutDriver merchantId city :<|> getDriverFleetGetAllVehicle merchantId city :<|> getDriverFleetGetAllDriver merchantId city :<|> postDriverFleetUnlink merchantId city :<|> postDriverFleetRemoveVehicle merchantId city :<|> postDriverFleetRemoveDriver merchantId city :<|> getDriverFleetTotalEarning merchantId city :<|> getDriverFleetVehicleEarning merchantId city :<|> getDriverFleetDriverEarning merchantId city :<|> getDriverFleetDriverVehicleAssociation merchantId city :<|> getDriverFleetDriverAssociation merchantId city :<|> getDriverFleetVehicleAssociation merchantId city :<|> postDriverFleetVehicleDriverRcStatus merchantId city :<|> postDriverUpdateFleetOwnerInfo merchantId city :<|> getDriverFleetOwnerInfo merchantId city :<|> postDriverFleetSendJoiningOtp merchantId city :<|> postDriverFleetVerifyJoiningOtp merchantId city :<|> getDriverFleetRoutes merchantId city :<|> getDriverFleetPossibleRoutes merchantId city :<|> postDriverFleetTripPlanner merchantId city :<|> getDriverFleetTripTransactions merchantId city :<|> postDriverFleetAddDrivers merchantId city :<|> postDriverFleetAddDriverBusRouteMapping merchantId city :<|> postDriverFleetLinkRCWithDriver merchantId city :<|> postDriverDashboardFleetWmbTripEnd merchantId city :<|> getDriverFleetWmbRouteDetails merchantId city :<|> postDriverDashboardFleetTrackDriver merchantId city

type PostDriverFleetAddVehicles =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_FLEET_ADD_VEHICLES)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetAddVehicles
  )

type PostDriverFleetAddVehicle =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_FLEET_ADD_VEHICLE)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetAddVehicle
  )

type GetDriverFleetGetDriverRequests =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.GET_DRIVER_FLEET_GET_DRIVER_REQUESTS)
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetGetDriverRequests
  )

type PostDriverFleetRespondDriverRequest =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_FLEET_RESPOND_DRIVER_REQUEST)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetRespondDriverRequest
  )

type PostDriverFleetAddRCWithoutDriver =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_FLEET_ADD_RC_WITHOUT_DRIVER)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetAddRCWithoutDriver
  )

type GetDriverFleetGetAllVehicle =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.GET_DRIVER_FLEET_GET_ALL_VEHICLE)
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetGetAllVehicle
  )

type GetDriverFleetGetAllDriver =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.GET_DRIVER_FLEET_GET_ALL_DRIVER)
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetGetAllDriver
  )

type PostDriverFleetUnlink =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_FLEET_UNLINK)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetUnlink
  )

type PostDriverFleetRemoveVehicle =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_FLEET_REMOVE_VEHICLE)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetRemoveVehicle
  )

type PostDriverFleetRemoveDriver =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_FLEET_REMOVE_DRIVER)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetRemoveDriver
  )

type GetDriverFleetTotalEarning =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.GET_DRIVER_FLEET_TOTAL_EARNING)
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetTotalEarning
  )

type GetDriverFleetVehicleEarning =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.GET_DRIVER_FLEET_VEHICLE_EARNING)
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetVehicleEarning
  )

type GetDriverFleetDriverEarning =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.GET_DRIVER_FLEET_DRIVER_EARNING)
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetDriverEarning
  )

type GetDriverFleetDriverVehicleAssociation =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.GET_DRIVER_FLEET_DRIVER_VEHICLE_ASSOCIATION)
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetDriverVehicleAssociation
  )

type GetDriverFleetDriverAssociation =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.GET_DRIVER_FLEET_DRIVER_ASSOCIATION)
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetDriverAssociation
  )

type GetDriverFleetVehicleAssociation =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.GET_DRIVER_FLEET_VEHICLE_ASSOCIATION)
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetVehicleAssociation
  )

type PostDriverFleetVehicleDriverRcStatus =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_FLEET_VEHICLE_DRIVER_RC_STATUS)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetVehicleDriverRcStatus
  )

type PostDriverUpdateFleetOwnerInfo =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_UPDATE_FLEET_OWNER_INFO)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverUpdateFleetOwnerInfo
  )

type GetDriverFleetOwnerInfo =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.GET_DRIVER_FLEET_OWNER_INFO)
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetOwnerInfo
  )

type PostDriverFleetSendJoiningOtp =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_FLEET_SEND_JOINING_OTP)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetSendJoiningOtp
  )

type PostDriverFleetVerifyJoiningOtp =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_FLEET_VERIFY_JOINING_OTP)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetVerifyJoiningOtp
  )

type GetDriverFleetRoutes =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.GET_DRIVER_FLEET_ROUTES)
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetRoutes
  )

type GetDriverFleetPossibleRoutes =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.GET_DRIVER_FLEET_POSSIBLE_ROUTES)
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetPossibleRoutes
  )

type PostDriverFleetTripPlanner =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_FLEET_TRIP_PLANNER)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetTripPlanner
  )

type GetDriverFleetTripTransactions =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.GET_DRIVER_FLEET_TRIP_TRANSACTIONS)
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetTripTransactions
  )

type PostDriverFleetAddDrivers =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_FLEET_ADD_DRIVERS)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetAddDrivers
  )

type PostDriverFleetAddDriverBusRouteMapping =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_FLEET_ADD_DRIVER_BUS_ROUTE_MAPPING)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetAddDriverBusRouteMapping
  )

type PostDriverFleetLinkRCWithDriver =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_FLEET_LINK_RC_WITH_DRIVER)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetLinkRCWithDriver
  )

type PostDriverDashboardFleetWmbTripEnd =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_DASHBOARD_FLEET_WMB_TRIP_END)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverDashboardFleetWmbTripEnd
  )

type GetDriverFleetWmbRouteDetails =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.GET_DRIVER_FLEET_WMB_ROUTE_DETAILS)
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetWmbRouteDetails
  )

type PostDriverDashboardFleetTrackDriver =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.DRIVER / 'API.Types.ProviderPlatform.Fleet.Driver.POST_DRIVER_DASHBOARD_FLEET_TRACK_DRIVER)
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverDashboardFleetTrackDriver
  )

postDriverFleetAddVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.Driver.CreateVehiclesReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities)
postDriverFleetAddVehicles merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetAddVehicles merchantShortId opCity apiTokenInfo req

postDriverFleetAddVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Driver.AddVehicleReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetAddVehicle merchantShortId opCity apiTokenInfo mobileNo countryCode req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetAddVehicle merchantShortId opCity apiTokenInfo mobileNo countryCode req

getDriverFleetGetDriverRequests :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Driver.RequestStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DriverRequestResp)
getDriverFleetGetDriverRequests merchantShortId opCity apiTokenInfo mbFrom mbTo mbStatus mbLimit mbOffset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetGetDriverRequests merchantShortId opCity apiTokenInfo mbFrom mbTo mbStatus mbLimit mbOffset

postDriverFleetRespondDriverRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.Driver.RequestRespondReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetRespondDriverRequest merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetRespondDriverRequest merchantShortId opCity apiTokenInfo req

postDriverFleetAddRCWithoutDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetAddRCWithoutDriver merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetAddRCWithoutDriver merchantShortId opCity apiTokenInfo req

getDriverFleetGetAllVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.ListVehicleRes)
getDriverFleetGetAllVehicle merchantShortId opCity apiTokenInfo mblimit mboffset mbRegNumberString = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetGetAllVehicle merchantShortId opCity apiTokenInfo mblimit mboffset mbRegNumberString

getDriverFleetGetAllDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetListDriverRes)
getDriverFleetGetAllDriver merchantShortId opCity apiTokenInfo mblimit mboffset mbMobileNumberString mbNameString = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetGetAllDriver merchantShortId opCity apiTokenInfo mblimit mboffset mbMobileNumberString mbNameString

postDriverFleetUnlink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetUnlink merchantShortId opCity apiTokenInfo driverId vehicleNo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetUnlink merchantShortId opCity apiTokenInfo driverId vehicleNo

postDriverFleetRemoveVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetRemoveVehicle merchantShortId opCity apiTokenInfo vehicleNo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetRemoveVehicle merchantShortId opCity apiTokenInfo vehicleNo

postDriverFleetRemoveDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetRemoveDriver merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetRemoveDriver merchantShortId opCity apiTokenInfo driverId

getDriverFleetTotalEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetTotalEarningResponse)
getDriverFleetTotalEarning merchantShortId opCity apiTokenInfo from to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetTotalEarning merchantShortId opCity apiTokenInfo from to

getDriverFleetVehicleEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes)
getDriverFleetVehicleEarning merchantShortId opCity apiTokenInfo vehicleNo limit offset from to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetVehicleEarning merchantShortId opCity apiTokenInfo vehicleNo limit offset from to

getDriverFleetDriverEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Driver.SortOn -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes)
getDriverFleetDriverEarning merchantShortId opCity apiTokenInfo mobileCountryCode mobileNo limit offset from to sortDesc sortOn = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetDriverEarning merchantShortId opCity apiTokenInfo mobileCountryCode mobileNo limit offset from to sortDesc sortOn

getDriverFleetDriverVehicleAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes)
getDriverFleetDriverVehicleAssociation merchantShortId opCity apiTokenInfo limit offset countryCode phoneNo vehicleNo includeStats from to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetDriverVehicleAssociation merchantShortId opCity apiTokenInfo limit offset countryCode phoneNo vehicleNo includeStats from to

getDriverFleetDriverAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Driver.DriverMode -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes)
getDriverFleetDriverAssociation merchantShortId opCity apiTokenInfo isActive limit offset countryCode phoneNo includeStats from to status name = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetDriverAssociation merchantShortId opCity apiTokenInfo isActive limit offset countryCode phoneNo includeStats from to status name

getDriverFleetVehicleAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Driver.FleetVehicleStatus -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes)
getDriverFleetVehicleAssociation merchantShortId opCity apiTokenInfo limit offset vehicleNo includeStats from to status = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetVehicleAssociation merchantShortId opCity apiTokenInfo limit offset vehicleNo includeStats from to status

postDriverFleetVehicleDriverRcStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.RCStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetVehicleDriverRcStatus merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetVehicleDriverRcStatus merchantShortId opCity apiTokenInfo driverId req

postDriverUpdateFleetOwnerInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.UpdateFleetOwnerInfoReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverUpdateFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId req

getDriverFleetOwnerInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetOwnerInfoRes)
getDriverFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId

postDriverFleetSendJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> Environment.FlowHandler Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes)
postDriverFleetSendJoiningOtp merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetSendJoiningOtp merchantShortId opCity apiTokenInfo req

postDriverFleetVerifyJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Driver.VerifyFleetJoiningOtpReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetVerifyJoiningOtp merchantShortId opCity apiTokenInfo authId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetVerifyJoiningOtp merchantShortId opCity apiTokenInfo authId req

getDriverFleetRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.RouteAPIResp)
getDriverFleetRoutes merchantShortId opCity apiTokenInfo mbCurrentLocation mbSearchString limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetRoutes merchantShortId opCity apiTokenInfo mbCurrentLocation mbSearchString limit offset

getDriverFleetPossibleRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.RouteAPIResp)
getDriverFleetPossibleRoutes merchantShortId opCity apiTokenInfo startStopCode = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetPossibleRoutes merchantShortId opCity apiTokenInfo startStopCode

postDriverFleetTripPlanner :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.Driver.TripPlannerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetTripPlanner merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetTripPlanner merchantShortId opCity apiTokenInfo req

getDriverFleetTripTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.TripTransactionResp)
getDriverFleetTripTransactions merchantShortId opCity apiTokenInfo driverId mbFrom mbTo mbVehicleNumber limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetTripTransactions merchantShortId opCity apiTokenInfo driverId mbFrom mbTo mbVehicleNumber limit offset

postDriverFleetAddDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.Driver.CreateDriversReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities)
postDriverFleetAddDrivers merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetAddDrivers merchantShortId opCity apiTokenInfo req

postDriverFleetAddDriverBusRouteMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.Driver.CreateDriverBusRouteMappingReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities)
postDriverFleetAddDriverBusRouteMapping merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetAddDriverBusRouteMapping merchantShortId opCity apiTokenInfo req

postDriverFleetLinkRCWithDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.Driver.LinkRCWithDriverForFleetReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetLinkRCWithDriver merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetLinkRCWithDriver merchantShortId opCity apiTokenInfo req

postDriverDashboardFleetWmbTripEnd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.TripTransaction -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverDashboardFleetWmbTripEnd merchantShortId opCity apiTokenInfo tripTransactionId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverDashboardFleetWmbTripEnd merchantShortId opCity apiTokenInfo tripTransactionId

getDriverFleetWmbRouteDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.RouteDetails)
getDriverFleetWmbRouteDetails merchantShortId opCity apiTokenInfo routeCode = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetWmbRouteDetails merchantShortId opCity apiTokenInfo routeCode

postDriverDashboardFleetTrackDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.Driver.TrackDriverLocationsReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.TrackDriverLocationsRes)
postDriverDashboardFleetTrackDriver merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverDashboardFleetTrackDriver merchantShortId opCity apiTokenInfo req
