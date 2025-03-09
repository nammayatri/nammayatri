{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Fleet.Driver
  ( API.Types.ProviderPlatform.Fleet.Driver.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified Dashboard.Common
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import qualified Domain.Action.Dashboard.Fleet.Driver as Domain.Action.Dashboard.Fleet.Driver
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Fleet.Driver.API)
handler merchantId city = postDriverFleetAddVehicles merchantId city :<|> postDriverFleetAddVehicle merchantId city :<|> getDriverFleetGetDriverRequests merchantId city :<|> postDriverFleetRespondDriverRequest merchantId city :<|> postDriverFleetAddRCWithoutDriver merchantId city :<|> getDriverFleetGetAllVehicle merchantId city :<|> getDriverFleetGetAllDriver merchantId city :<|> postDriverFleetUnlink merchantId city :<|> postDriverFleetRemoveVehicle merchantId city :<|> postDriverFleetRemoveDriver merchantId city :<|> getDriverFleetTotalEarning merchantId city :<|> getDriverFleetVehicleEarning merchantId city :<|> getDriverFleetDriverEarning merchantId city :<|> getDriverFleetDriverVehicleAssociation merchantId city :<|> getDriverFleetDriverAssociation merchantId city :<|> getDriverFleetVehicleAssociation merchantId city :<|> postDriverFleetVehicleDriverRcStatus merchantId city :<|> postDriverUpdateFleetOwnerInfo merchantId city :<|> getDriverFleetOwnerInfo merchantId city :<|> postDriverFleetSendJoiningOtp merchantId city :<|> postDriverFleetVerifyJoiningOtp merchantId city :<|> getDriverFleetRoutes merchantId city :<|> getDriverFleetPossibleRoutes merchantId city :<|> postDriverFleetTripPlanner merchantId city :<|> getDriverFleetTripTransactions merchantId city :<|> postDriverFleetAddDrivers merchantId city :<|> postDriverFleetAddDriverBusRouteMapping merchantId city :<|> postDriverFleetLinkRCWithDriver merchantId city :<|> postDriverDashboardFleetWmbTripEnd merchantId city :<|> getDriverFleetWmbRouteDetails merchantId city :<|> postDriverDashboardFleetTrackDriver merchantId city

postDriverFleetAddVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Fleet.Driver.CreateVehiclesReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities)
postDriverFleetAddVehicles a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddVehicles a3 a2 a1

postDriverFleetAddVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Driver.AddVehicleReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetAddVehicle a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddVehicle a6 a5 a4 a3 a2 a1

getDriverFleetGetDriverRequests :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Driver.RequestStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DriverRequestResp)
getDriverFleetGetDriverRequests a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetGetDriverRequests a8 a7 a6 a5 a4 a3 a2 a1

postDriverFleetRespondDriverRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Driver.RequestRespondReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetRespondDriverRequest a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetRespondDriverRequest a4 a3 a2 a1

postDriverFleetAddRCWithoutDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetAddRCWithoutDriver a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddRCWithoutDriver a4 a3 a2 a1

getDriverFleetGetAllVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.ListVehicleRes)
getDriverFleetGetAllVehicle a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetGetAllVehicle a6 a5 a4 a3 a2 a1

getDriverFleetGetAllDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetListDriverRes)
getDriverFleetGetAllDriver a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetGetAllDriver a7 a6 a5 a4 a3 a2 a1

postDriverFleetUnlink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetUnlink a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetUnlink a5 a4 a3 a2 a1

postDriverFleetRemoveVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetRemoveVehicle a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetRemoveVehicle a4 a3 a2 a1

postDriverFleetRemoveDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetRemoveDriver a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetRemoveDriver a4 a3 a2 a1

getDriverFleetTotalEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetTotalEarningResponse)
getDriverFleetTotalEarning a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetTotalEarning a5 a4 a3 a2 a1

getDriverFleetVehicleEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes)
getDriverFleetVehicleEarning a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetVehicleEarning a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetDriverEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Driver.SortOn -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes)
getDriverFleetDriverEarning a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverEarning a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetDriverVehicleAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes)
getDriverFleetDriverVehicleAssociation a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverVehicleAssociation a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetDriverAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Driver.DriverMode -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes)
getDriverFleetDriverAssociation a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverAssociation a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetVehicleAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Driver.FleetVehicleStatus -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes)
getDriverFleetVehicleAssociation a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetVehicleAssociation a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

postDriverFleetVehicleDriverRcStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Driver.RCStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetVehicleDriverRcStatus a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetVehicleDriverRcStatus a5 a4 a3 a2 a1

postDriverUpdateFleetOwnerInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.UpdateFleetOwnerInfoReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateFleetOwnerInfo a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverUpdateFleetOwnerInfo a4 a3 a2 a1

getDriverFleetOwnerInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetOwnerInfoRes)
getDriverFleetOwnerInfo a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetOwnerInfo a3 a2 a1

postDriverFleetSendJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> Environment.FlowHandler Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes)
postDriverFleetSendJoiningOtp a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetSendJoiningOtp a4 a3 a2 a1

postDriverFleetVerifyJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Driver.VerifyFleetJoiningOtpReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetVerifyJoiningOtp a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetVerifyJoiningOtp a5 a4 a3 a2 a1

getDriverFleetRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.RouteAPIResp)
getDriverFleetRoutes a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetRoutes a7 a6 a5 a4 a3 a2 a1

getDriverFleetPossibleRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.RouteAPIResp)
getDriverFleetPossibleRoutes a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetPossibleRoutes a4 a3 a2 a1

postDriverFleetTripPlanner :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Driver.TripPlannerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetTripPlanner a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetTripPlanner a4 a3 a2 a1

getDriverFleetTripTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.TripTransactionResp)
getDriverFleetTripTransactions a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetTripTransactions a9 a8 a7 a6 a5 a4 a3 a2 a1

postDriverFleetAddDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Fleet.Driver.CreateDriversReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities)
postDriverFleetAddDrivers a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddDrivers a3 a2 a1

postDriverFleetAddDriverBusRouteMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Fleet.Driver.CreateDriverBusRouteMappingReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities)
postDriverFleetAddDriverBusRouteMapping a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddDriverBusRouteMapping a3 a2 a1

postDriverFleetLinkRCWithDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Driver.LinkRCWithDriverForFleetReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetLinkRCWithDriver a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetLinkRCWithDriver a4 a3 a2 a1

postDriverDashboardFleetWmbTripEnd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.TripTransaction -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverDashboardFleetWmbTripEnd a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverDashboardFleetWmbTripEnd a4 a3 a2 a1

getDriverFleetWmbRouteDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.RouteDetails)
getDriverFleetWmbRouteDetails a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetWmbRouteDetails a4 a3 a2 a1

postDriverDashboardFleetTrackDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Driver.TrackDriverLocationsReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.TrackDriverLocationsRes)
postDriverDashboardFleetTrackDriver a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverDashboardFleetTrackDriver a4 a3 a2 a1
