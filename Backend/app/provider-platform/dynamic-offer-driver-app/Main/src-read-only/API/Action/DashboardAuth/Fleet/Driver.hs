{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Fleet.Driver
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import qualified Data.Time
import qualified Domain.Action.Dashboard.Fleet.Driver
import qualified Domain.Types.Alert.AlertRequestStatus
import qualified Domain.Types.Alert.AlertRequestType
import qualified Domain.Types.FleetBadgeType
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.Fleet
import Tools.Auth
import qualified Tools.Auth.DashboardRegistration
import Tools.Auth.DashboardUserAuth

type API = ("driver" :> (GetDriverFleetAccessList :<|> GetDriverFleetOwnerList :<|> PostDriverFleetAccessSelect :<|> PostDriverFleetV2AccessSelect :<|> PostDriverFleetV2AccessMultiOwnerIdSelect :<|> PostDriverFleetAddVehicles :<|> PostDriverAddRidePayoutAccountNumber :<|> PostDriverFleetAddVehicle :<|> GetDriverFleetGetDriverRequests :<|> PostDriverFleetRespondDriverRequest :<|> PostDriverFleetAddRCWithoutDriver :<|> GetDriverFleetGetAllVehicle :<|> GetDriverFleetGetAllDriver :<|> GetDriverFleetGetAllBadge :<|> PostDriverFleetUnlink :<|> PostDriverFleetRemoveVehicle :<|> PostDriverFleetCashRideUpdate :<|> PostDriverFleetRemoveDriver :<|> GetDriverFleetTotalEarning :<|> GetDriverFleetVehicleEarning :<|> GetDriverFleetDriverEarning :<|> GetDriverFleetBookings :<|> GetDriverFleetAssignments :<|> GetDriverFleetDriverVehicleAssociation :<|> GetDriverFleetDriverListStats :<|> GetDriverFleetDriverAssociation :<|> GetDriverFleetVehicleAssociation :<|> PostDriverFleetVehicleEdit :<|> PostDriverFleetVehicleDriverRcStatus :<|> PostDriverUpdateFleetOwnerInfo :<|> GetDriverFleetOwnerInfo :<|> GetDriverFleetOperatorInfo :<|> PostDriverFleetSendJoiningOtp :<|> PostDriverFleetVerifyJoiningOtp :<|> GetDriverFleetRoutes :<|> GetDriverFleetPossibleRoutes :<|> PostDriverFleetTripPlanner :<|> GetDriverFleetTripTransactions :<|> PostDriverFleetAddDrivers :<|> PostDriverFleetAddDriverBusRouteMapping :<|> PostDriverFleetLinkRCWithDriver :<|> PostDriverDashboardFleetWmbTripEnd :<|> GetDriverDashboardFleetTripWaypointsHelper :<|> GetDriverFleetWmbRouteDetails :<|> PostDriverFleetGetNearbyDrivers :<|> PostDriverDashboardFleetTrackDriver :<|> GetDriverDashboardInternalHelperGetFleetOwnerId :<|> GetDriverDashboardInternalHelperGetFleetOwnerIds :<|> GetDriverFleetStatus :<|> PostDriverFleetLocationList :<|> PostDriverFleetGetDriverDetails :<|> PostDriverFleetGetNearbyDriversV2 :<|> GetDriverFleetDashboardAnalyticsAllTime :<|> GetDriverFleetDashboardAnalytics :<|> PostDriverFleetDashboardAnalyticsCache :<|> PostDriverDashboardFleetEstimateRoute :<|> PostDriverFleetApproveDriver :<|> PostDriverFleetDriverUpdate :<|> PostDriverFleetDriverChangeFleetOwner :<|> PostDriverFleetVehicleChangeFleetOwner :<|> GetDriverFleetDriverDetails :<|> PostDriverFleetTripTransactionsV2 :<|> GetDriverFleetVehicleListStats :<|> GetDriverFleetDriverOnboardedDriversAndUnlinkedVehicles :<|> GetDriverFleetStatusSummary :<|> GetDriverFleetScheduledBookingList :<|> PostDriverFleetScheduledBookingAssign :<|> PostDriverFleetScheduledBookingCancel :<|> PostDriverFleetScheduledBookingReassign :<|> GetDriverVehicleInfo))

type GetDriverFleetAccessList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_ACCESS_LIST"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetAccessList
  )

type GetDriverFleetOwnerList = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_OWNER_LIST" :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetOwnerList)

type PostDriverFleetAccessSelect =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ACCESS_SELECT"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetAccessSelect
  )

type PostDriverFleetV2AccessSelect =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_V2_ACCESS_SELECT"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetV2AccessSelect
  )

type PostDriverFleetV2AccessMultiOwnerIdSelect =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_V2_ACCESS_MULTI_OWNER_ID_SELECT"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetV2AccessMultiOwnerIdSelect
  )

type PostDriverFleetAddVehicles =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_VEHICLES"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetAddVehicles
  )

type PostDriverAddRidePayoutAccountNumber =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_ADD_RIDE_PAYOUT_ACCOUNT_NUMBER"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverAddRidePayoutAccountNumber
  )

type PostDriverFleetAddVehicle =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_VEHICLE"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetAddVehicle
  )

type GetDriverFleetGetDriverRequests =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_GET_DRIVER_REQUESTS"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetGetDriverRequests
  )

type PostDriverFleetRespondDriverRequest =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_RESPOND_DRIVER_REQUEST"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetRespondDriverRequest
  )

type PostDriverFleetAddRCWithoutDriver =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_RC_WITHOUT_DRIVER"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetAddRCWithoutDriver
  )

type GetDriverFleetGetAllVehicle =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_GET_ALL_VEHICLE"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetGetAllVehicle
  )

type GetDriverFleetGetAllDriver =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_GET_ALL_DRIVER"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetGetAllDriver
  )

type GetDriverFleetGetAllBadge =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_GET_ALL_BADGE"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetGetAllBadge
  )

type PostDriverFleetUnlink =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_UNLINK"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetUnlink
  )

type PostDriverFleetRemoveVehicle =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_REMOVE_VEHICLE"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetRemoveVehicle
  )

type PostDriverFleetCashRideUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_CASH_RIDE_UPDATE"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetCashRideUpdate
  )

type PostDriverFleetRemoveDriver =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_REMOVE_DRIVER"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetRemoveDriver
  )

type GetDriverFleetTotalEarning =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_TOTAL_EARNING"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetTotalEarning
  )

type GetDriverFleetVehicleEarning =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_VEHICLE_EARNING"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetVehicleEarning
  )

type GetDriverFleetDriverEarning =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DRIVER_EARNING"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetDriverEarning
  )

type GetDriverFleetBookings =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_BOOKINGS"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetBookings
  )

type GetDriverFleetAssignments =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_ASSIGNMENTS"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetAssignments
  )

type GetDriverFleetDriverVehicleAssociation =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DRIVER_VEHICLE_ASSOCIATION"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetDriverVehicleAssociation
  )

type GetDriverFleetDriverListStats =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DRIVER_LIST_STATS"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetDriverListStats
  )

type GetDriverFleetDriverAssociation =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DRIVER_ASSOCIATION"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetDriverAssociation
  )

type GetDriverFleetVehicleAssociation =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_VEHICLE_ASSOCIATION"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetVehicleAssociation
  )

type PostDriverFleetVehicleEdit =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VEHICLE_EDIT"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetVehicleEdit
  )

type PostDriverFleetVehicleDriverRcStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VEHICLE_DRIVER_RC_STATUS"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetVehicleDriverRcStatus
  )

type PostDriverUpdateFleetOwnerInfo =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_UPDATE_FLEET_OWNER_INFO"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverUpdateFleetOwnerInfo
  )

type GetDriverFleetOwnerInfo = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_OWNER_INFO" :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetOwnerInfo)

type GetDriverFleetOperatorInfo =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_OPERATOR_INFO"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetOperatorInfo
  )

type PostDriverFleetSendJoiningOtp =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_SEND_JOINING_OTP"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetSendJoiningOtp
  )

type PostDriverFleetVerifyJoiningOtp =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VERIFY_JOINING_OTP"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetVerifyJoiningOtp
  )

type GetDriverFleetRoutes =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_ROUTES"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetRoutes
  )

type GetDriverFleetPossibleRoutes =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_POSSIBLE_ROUTES"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetPossibleRoutes
  )

type PostDriverFleetTripPlanner =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_TRIP_PLANNER"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetTripPlanner
  )

type GetDriverFleetTripTransactions =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_TRIP_TRANSACTIONS"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetTripTransactions
  )

type PostDriverFleetAddDrivers =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_DRIVERS"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetAddDrivers
  )

type PostDriverFleetAddDriverBusRouteMapping =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_DRIVER_BUS_ROUTE_MAPPING"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetAddDriverBusRouteMapping
  )

type PostDriverFleetLinkRCWithDriver =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_LINK_RC_WITH_DRIVER"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetLinkRCWithDriver
  )

type PostDriverDashboardFleetWmbTripEnd =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_DASHBOARD_FLEET_WMB_TRIP_END"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverDashboardFleetWmbTripEnd
  )

type GetDriverDashboardFleetTripWaypointsHelper =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_DASHBOARD_FLEET_TRIP_WAYPOINTS"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverDashboardFleetTripWaypointsHelper
  )

type GetDriverFleetWmbRouteDetails =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_WMB_ROUTE_DETAILS"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetWmbRouteDetails
  )

type PostDriverFleetGetNearbyDrivers =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_GET_NEARBY_DRIVERS"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetGetNearbyDrivers
  )

type PostDriverDashboardFleetTrackDriver =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_DASHBOARD_FLEET_TRACK_DRIVER"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverDashboardFleetTrackDriver
  )

type GetDriverDashboardInternalHelperGetFleetOwnerId = API.Types.ProviderPlatform.Fleet.Driver.GetDriverDashboardInternalHelperGetFleetOwnerId

type GetDriverDashboardInternalHelperGetFleetOwnerIds = API.Types.ProviderPlatform.Fleet.Driver.GetDriverDashboardInternalHelperGetFleetOwnerIds

type GetDriverFleetStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_STATUS"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetStatus
  )

type PostDriverFleetLocationList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_LOCATION_LIST"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetLocationList
  )

type PostDriverFleetGetDriverDetails =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_GET_DRIVER_DETAILS"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetGetDriverDetails
  )

type PostDriverFleetGetNearbyDriversV2 =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_GET_NEARBY_DRIVERS_V2"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetGetNearbyDriversV2
  )

type GetDriverFleetDashboardAnalyticsAllTime =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DASHBOARD_ANALYTICS_ALL_TIME"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetDashboardAnalyticsAllTime
  )

type GetDriverFleetDashboardAnalytics =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DASHBOARD_ANALYTICS"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetDashboardAnalytics
  )

type PostDriverFleetDashboardAnalyticsCache =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_DASHBOARD_ANALYTICS_CACHE"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetDashboardAnalyticsCache
  )

type PostDriverDashboardFleetEstimateRoute =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_DASHBOARD_FLEET_ESTIMATE_ROUTE"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverDashboardFleetEstimateRoute
  )

type PostDriverFleetApproveDriver =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_APPROVE_DRIVER"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetApproveDriver
  )

type PostDriverFleetDriverUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_DRIVER_UPDATE"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetDriverUpdate
  )

type PostDriverFleetDriverChangeFleetOwner =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_DRIVER_CHANGE_FLEET_OWNER"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetDriverChangeFleetOwner
  )

type PostDriverFleetVehicleChangeFleetOwner =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VEHICLE_CHANGE_FLEET_OWNER"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetVehicleChangeFleetOwner
  )

type GetDriverFleetDriverDetails =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DRIVER_DETAILS"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetDriverDetails
  )

type PostDriverFleetTripTransactionsV2 =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_TRIP_TRANSACTIONS_V2"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetTripTransactionsV2
  )

type GetDriverFleetVehicleListStats =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_VEHICLE_LIST_STATS"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetVehicleListStats
  )

type GetDriverFleetDriverOnboardedDriversAndUnlinkedVehicles =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DRIVER_ONBOARDED_DRIVERS_AND_UNLINKED_VEHICLES"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetDriverOnboardedDriversAndUnlinkedVehicles
  )

type GetDriverFleetStatusSummary =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_STATUS_SUMMARY"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetStatusSummary
  )

type GetDriverFleetScheduledBookingList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_SCHEDULED_BOOKING_LIST"
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetScheduledBookingList
  )

type PostDriverFleetScheduledBookingAssign =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_SCHEDULED_BOOKING_ASSIGN"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetScheduledBookingAssign
  )

type PostDriverFleetScheduledBookingCancel =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_SCHEDULED_BOOKING_CANCEL"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetScheduledBookingCancel
  )

type PostDriverFleetScheduledBookingReassign =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_SCHEDULED_BOOKING_REASSIGN"
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetScheduledBookingReassign
  )

type GetDriverVehicleInfo = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_FLEET/DRIVER/GET_DRIVER_VEHICLE_INFO" :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverVehicleInfo)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverFleetAccessList merchantId city :<|> getDriverFleetOwnerList merchantId city :<|> postDriverFleetAccessSelect merchantId city :<|> postDriverFleetV2AccessSelect merchantId city :<|> postDriverFleetV2AccessMultiOwnerIdSelect merchantId city :<|> postDriverFleetAddVehicles merchantId city :<|> postDriverAddRidePayoutAccountNumber merchantId city :<|> postDriverFleetAddVehicle merchantId city :<|> getDriverFleetGetDriverRequests merchantId city :<|> postDriverFleetRespondDriverRequest merchantId city :<|> postDriverFleetAddRCWithoutDriver merchantId city :<|> getDriverFleetGetAllVehicle merchantId city :<|> getDriverFleetGetAllDriver merchantId city :<|> getDriverFleetGetAllBadge merchantId city :<|> postDriverFleetUnlink merchantId city :<|> postDriverFleetRemoveVehicle merchantId city :<|> postDriverFleetCashRideUpdate merchantId city :<|> postDriverFleetRemoveDriver merchantId city :<|> getDriverFleetTotalEarning merchantId city :<|> getDriverFleetVehicleEarning merchantId city :<|> getDriverFleetDriverEarning merchantId city :<|> getDriverFleetBookings merchantId city :<|> getDriverFleetAssignments merchantId city :<|> getDriverFleetDriverVehicleAssociation merchantId city :<|> getDriverFleetDriverListStats merchantId city :<|> getDriverFleetDriverAssociation merchantId city :<|> getDriverFleetVehicleAssociation merchantId city :<|> postDriverFleetVehicleEdit merchantId city :<|> postDriverFleetVehicleDriverRcStatus merchantId city :<|> postDriverUpdateFleetOwnerInfo merchantId city :<|> getDriverFleetOwnerInfo merchantId city :<|> getDriverFleetOperatorInfo merchantId city :<|> postDriverFleetSendJoiningOtp merchantId city :<|> postDriverFleetVerifyJoiningOtp merchantId city :<|> getDriverFleetRoutes merchantId city :<|> getDriverFleetPossibleRoutes merchantId city :<|> postDriverFleetTripPlanner merchantId city :<|> getDriverFleetTripTransactions merchantId city :<|> postDriverFleetAddDrivers merchantId city :<|> postDriverFleetAddDriverBusRouteMapping merchantId city :<|> postDriverFleetLinkRCWithDriver merchantId city :<|> postDriverDashboardFleetWmbTripEnd merchantId city :<|> getDriverDashboardFleetTripWaypoints merchantId city :<|> getDriverFleetWmbRouteDetails merchantId city :<|> postDriverFleetGetNearbyDrivers merchantId city :<|> postDriverDashboardFleetTrackDriver merchantId city :<|> getDriverDashboardInternalHelperGetFleetOwnerId merchantId city :<|> getDriverDashboardInternalHelperGetFleetOwnerIds merchantId city :<|> getDriverFleetStatus merchantId city :<|> postDriverFleetLocationList merchantId city :<|> postDriverFleetGetDriverDetails merchantId city :<|> postDriverFleetGetNearbyDriversV2 merchantId city :<|> getDriverFleetDashboardAnalyticsAllTime merchantId city :<|> getDriverFleetDashboardAnalytics merchantId city :<|> postDriverFleetDashboardAnalyticsCache merchantId city :<|> postDriverDashboardFleetEstimateRoute merchantId city :<|> postDriverFleetApproveDriver merchantId city :<|> postDriverFleetDriverUpdate merchantId city :<|> postDriverFleetDriverChangeFleetOwner merchantId city :<|> postDriverFleetVehicleChangeFleetOwner merchantId city :<|> getDriverFleetDriverDetails merchantId city :<|> postDriverFleetTripTransactionsV2 merchantId city :<|> getDriverFleetVehicleListStats merchantId city :<|> getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles merchantId city :<|> getDriverFleetStatusSummary merchantId city :<|> getDriverFleetScheduledBookingList merchantId city :<|> postDriverFleetScheduledBookingAssign merchantId city :<|> postDriverFleetScheduledBookingCancel merchantId city :<|> postDriverFleetScheduledBookingReassign merchantId city :<|> getDriverVehicleInfo merchantId city

getDriverFleetAccessList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetOwnerListRes)
getDriverFleetAccessList a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetAccessList a4 a3 a1

getDriverFleetOwnerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Dashboard.Common.Driver.ApprovalStatusFilter) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.DocsVerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetType) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler [API.Types.ProviderPlatform.Fleet.Driver.FleetOwnerListItem])
getDriverFleetOwnerList a15 a14 _a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetOwnerList a15 a14 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

postDriverFleetAccessSelect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Bool -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetAccessSelect a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAccessSelect a7 a6 a4 a3 a2 a1

postDriverFleetV2AccessSelect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Bool -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetV2AccessSelect a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetV2AccessSelect a8 a7 a5 a4 a3 a2 a1

postDriverFleetV2AccessMultiOwnerIdSelect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Bool -> API.Types.ProviderPlatform.Fleet.Driver.MultiOwnerSelect -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetV2AccessMultiOwnerIdSelect a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetV2AccessMultiOwnerIdSelect a7 a6 a4 a3 a2 a1

postDriverFleetAddVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.CreateVehiclesReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities)
postDriverFleetAddVehicles a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a3
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a3) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a3) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddVehicles a5 a4 a1 {API.Types.ProviderPlatform.Fleet.Driver.fleetOwnerId = mbFleetOwnerId, API.Types.ProviderPlatform.Fleet.Driver.requestorId = Kernel.Prelude.Just requestorId}

postDriverAddRidePayoutAccountNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.AddRidePayoutAccountNumberReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverAddRidePayoutAccountNumber a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverAddRidePayoutAccountNumber a4 a3 a1

postDriverFleetAddVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Dashboard.Common.Role) -> API.Types.ProviderPlatform.Fleet.Driver.AddVehicleReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetAddVehicle a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a6
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a6) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a6) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6) a3
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddVehicle a8 a7 a5 requestorId mbFleetOwnerId a4 a2 a1

getDriverFleetGetDriverRequests :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Domain.Types.Alert.AlertRequestType.AlertRequestType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.Alert.AlertRequestStatus.AlertRequestStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DriverRequestRespT)
getDriverFleetGetDriverRequests a13 a12 _a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetGetDriverRequests a13 a12 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

postDriverFleetRespondDriverRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.RequestRespondReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetRespondDriverRequest a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetRespondDriverRequest a5 a4 fleetOwnerId a1

postDriverFleetAddRCWithoutDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetAddRCWithoutDriver a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a3
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddRCWithoutDriver a5 a4 fleetOwnerId a1

getDriverFleetGetAllVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.Driver.ApprovalStatusFilter) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.ListVehicleResT)
getDriverFleetGetAllVehicle a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a11
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetGetAllVehicle a13 a12 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetGetAllDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.Driver.ApprovalStatusFilter) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.Driver.OnboardingAs) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetListDriverResT)
getDriverFleetGetAllDriver a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a15
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetGetAllDriver a17 a16 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetGetAllBadge :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.FleetBadgeType.FleetBadgeType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetBadgeResT)
getDriverFleetGetAllBadge a10 a9 _a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetGetAllBadge a10 a9 a7 a6 a5 a4 a3 a2 a1

postDriverFleetUnlink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetUnlink a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a4
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a4) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a4) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) a1
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetUnlink a6 a5 requestorId a3 a2 mbFleetOwnerId

postDriverFleetRemoveVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetRemoveVehicle a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a3
  (fleetOwnerId, requestorId) <- SharedLogic.Fleet.getFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a3) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a3) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetRemoveVehicle a5 a4 fleetOwnerId a2 (Kernel.Prelude.Just requestorId)

postDriverFleetRemoveDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetRemoveDriver a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a4
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a4) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a4) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetRemoveDriver a6 a5 requestorId a3 mbFleetOwnerId a1

getDriverFleetTotalEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetTotalEarningResponse)
getDriverFleetTotalEarning a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetTotalEarning a5 a4 fleetOwnerId a2 a1

getDriverFleetVehicleEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes)
getDriverFleetVehicleEarning a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetVehicleEarning a8 a7 fleetOwnerId a5 a4 a3 a2 a1

getDriverFleetDriverEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.SortOn) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes)
getDriverFleetDriverEarning a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a9) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverEarning a11 a10 fleetOwnerId a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetBookings :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetBookingsInformationResponse)
getDriverFleetBookings a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetBookings a11 a10 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a9) a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetAssignments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetBookingAssignmentsResponse)
getDriverFleetAssignments a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetAssignments a10 a9 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a8) a7 a6 a5 a4 a3 a2 a1

getDriverFleetDriverVehicleAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes)
getDriverFleetDriverVehicleAssociation a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a9
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a9) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverVehicleAssociation a11 a10 fleetOwnerId a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetDriverListStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.FleetDriverListStatsSortOn) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.FleetDriverStatsResponseType) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetDriverStatsListRes)
getDriverFleetDriverListStats a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a12) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a12) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a12) a9
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverListStats a14 a13 requestorId a11 a10 mbFleetOwnerId a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetDriverAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.DriverMode) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.DocsVerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationResT)
getDriverFleetDriverAssociation a25 a24 a23 a22 a21 a20 a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a23
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverAssociation a25 a24 a22 a21 a20 a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetVehicleAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.FleetVehicleStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.DocsVerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationResT)
getDriverFleetVehicleAssociation a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a17
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetVehicleAssociation a19 a18 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

postDriverFleetVehicleEdit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.EditVehicleReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetVehicleEdit a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a6
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a6) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a6) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6) a5
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetVehicleEdit a8 a7 requestorId mbFleetOwnerId a4 a3 a2 a1

postDriverFleetVehicleDriverRcStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.RCStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetVehicleDriverRcStatus a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a4
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a4) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a4) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetVehicleDriverRcStatus a6 a5 a3 requestorId mbFleetOwnerId a1

postDriverUpdateFleetOwnerInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.UpdateFleetOwnerInfoReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateFleetOwnerInfo a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) a2
  Kernel.Prelude.unless (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4 Kernel.Prelude.== a3.getId Kernel.Prelude.&& fleetOwnerId Kernel.Prelude.== Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) $
    Kernel.Utils.Common.throwError Kernel.Types.Error.AccessDenied
  Domain.Action.Dashboard.Fleet.Driver.postDriverUpdateFleetOwnerInfo a6 a5 (Kernel.Types.Id.Id fleetOwnerId) a1

getDriverFleetOwnerInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetOwnerInfoRes)
getDriverFleetOwnerInfo a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetOwnerInfo a4 a3 a1

getDriverFleetOperatorInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetOwnerInfoRes)
getDriverFleetOperatorInfo a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  res <- Domain.Action.Dashboard.Fleet.Driver.getDriverFleetOperatorInfo a7 a6 a4 a3 a2 a1
  mbApprovedBy <- Tools.Auth.DashboardRegistration.dashboardPersonApprovedBy res.id
  Kernel.Prelude.pure res {API.Types.ProviderPlatform.Fleet.Driver.approvedBy = maybe res.approvedBy Kernel.Prelude.Just mbApprovedBy}

postDriverFleetSendJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> Environment.FlowHandler Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes)
postDriverFleetSendJoiningOtp a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  (fleetOwnerId, requestorId) <- SharedLogic.Fleet.getFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a3) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a3) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetSendJoiningOtp a5 a4 (Tools.Auth.DashboardUserAuth.dashboardRequestorName a3) (Kernel.Prelude.Just fleetOwnerId) (Kernel.Prelude.Just requestorId) a1

postDriverFleetVerifyJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.VerifyFleetJoiningOtpReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetVerifyJoiningOtp a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  (fleetOwnerId, requestorId) <- SharedLogic.Fleet.getFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a4) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a4) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetVerifyJoiningOtp a6 a5 fleetOwnerId a3 (Kernel.Prelude.Just requestorId) a1

getDriverFleetRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.External.Maps.Types.LatLong) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.RouteAPIResp)
getDriverFleetRoutes a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6) a3
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetRoutes a8 a7 fleetOwnerId a5 a4 a2 a1

getDriverFleetPossibleRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.RouteAPIResp)
getDriverFleetPossibleRoutes a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetPossibleRoutes a5 a4 fleetOwnerId a1

postDriverFleetTripPlanner :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.TripPlannerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetTripPlanner a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetTripPlanner a5 a4 fleetOwnerId a1

getDriverFleetTripTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.TripTransactionRespT)
getDriverFleetTripTransactions a11 a10 _a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetTripTransactions a11 a10 a8 a7 a6 a5 a4 a3 a2 a1

postDriverFleetAddDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.CreateDriversReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.AddDriversResp)
postDriverFleetAddDrivers a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a3
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a3) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a3) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddDrivers a5 a4 (Kernel.Prelude.Just requestorId) a1 {API.Types.ProviderPlatform.Fleet.Driver.fleetOwnerId = mbFleetOwnerId}

postDriverFleetAddDriverBusRouteMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.CreateDriverBusRouteMappingReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities)
postDriverFleetAddDriverBusRouteMapping a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddDriverBusRouteMapping a5 a4 a1 {API.Types.ProviderPlatform.Fleet.Driver.fleetOwnerId = Kernel.Prelude.Just fleetOwnerId}

postDriverFleetLinkRCWithDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.LinkRCWithDriverForFleetReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetLinkRCWithDriver a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a3
  (fleetOwnerId, requestorId) <- SharedLogic.Fleet.getFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a3) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a3) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetLinkRCWithDriver a5 a4 fleetOwnerId (Kernel.Prelude.Just requestorId) a1

postDriverDashboardFleetWmbTripEnd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.TripTransaction -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Dashboard.Common.ActionSource) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverDashboardFleetWmbTripEnd a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverDashboardFleetWmbTripEnd a6 a5 a3 fleetOwnerId a1

getDriverDashboardFleetTripWaypoints :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.TripTransaction -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.TripTransactionWaypointsRes)
getDriverDashboardFleetTripWaypoints a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverDashboardFleetTripWaypoints a7 a6 a4 a3 a2 a1

getDriverFleetWmbRouteDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.RouteDetails)
getDriverFleetWmbRouteDetails a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetWmbRouteDetails a5 a4 fleetOwnerId a2

postDriverFleetGetNearbyDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.NearbyDriverReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.NearbyDriverRespT)
postDriverFleetGetNearbyDrivers a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerIds <- SharedLogic.Fleet.getFleetOwnerIds (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) Kernel.Prelude.Nothing
  drivers <-
    concatMapM
      ( \(fleetOwnerId, fleetOwnerName) -> do
          API.Types.ProviderPlatform.Fleet.Driver.NearbyDriverResp {..} <- Domain.Action.Dashboard.Fleet.Driver.postDriverFleetGetNearbyDrivers a4 a3 fleetOwnerId a1
          Kernel.Prelude.pure $ Kernel.Prelude.map (\API.Types.ProviderPlatform.Fleet.Driver.DriverInfo {..} -> API.Types.ProviderPlatform.Fleet.Driver.DriverInfoT {..}) drivers
      )
      fleetOwnerIds
  Kernel.Prelude.pure API.Types.ProviderPlatform.Fleet.Driver.NearbyDriverRespT {..}

postDriverDashboardFleetTrackDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.TrackDriverLocationsReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.TrackDriverLocationsRes)
postDriverDashboardFleetTrackDriver a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverDashboardFleetTrackDriver a5 a4 fleetOwnerId a1

getDriverDashboardInternalHelperGetFleetOwnerId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Prelude.Text)
getDriverDashboardInternalHelperGetFleetOwnerId a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverDashboardInternalHelperGetFleetOwnerId a4 a3 a2 a1

getDriverDashboardInternalHelperGetFleetOwnerIds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Text -> Environment.FlowHandler [(Kernel.Prelude.Text, Kernel.Prelude.Text)])
getDriverDashboardInternalHelperGetFleetOwnerIds a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverDashboardInternalHelperGetFleetOwnerIds a4 a3 a2 a1

getDriverFleetStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DriverStatusRes)
getDriverFleetStatus a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a2) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a2) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetStatus a4 a3 requestorId mbFleetOwnerId

postDriverFleetLocationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.DriverLocationListReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DriverLocationListResp)
postDriverFleetLocationList a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetLocationList a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

postDriverFleetGetDriverDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.DriverDetailsReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DriverDetailsResp)
postDriverFleetGetDriverDetails a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetGetDriverDetails a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

postDriverFleetGetNearbyDriversV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.NearbyDriversReqV2 -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.NearbyDriversRespTV2)
postDriverFleetGetNearbyDriversV2 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerIds <- SharedLogic.Fleet.getFleetOwnerIds (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) Kernel.Prelude.Nothing
  drivers <-
    concatMapM
      ( \(fleetOwnerId, fleetOwnerName) -> do
          API.Types.ProviderPlatform.Fleet.Driver.NearbyDriversRespV2 {..} <- Domain.Action.Dashboard.Fleet.Driver.postDriverFleetGetNearbyDriversV2 a4 a3 fleetOwnerId a1
          Kernel.Prelude.pure $ Kernel.Prelude.map (\API.Types.ProviderPlatform.Fleet.Driver.NearbyDriverDetails {..} -> API.Types.ProviderPlatform.Fleet.Driver.NearbyDriverDetailsT {..}) drivers
      )
      fleetOwnerIds
  Kernel.Prelude.pure API.Types.ProviderPlatform.Fleet.Driver.NearbyDriversRespTV2 {..}

getDriverFleetDashboardAnalyticsAllTime :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.AllTimeFleetAnalyticsRes)
getDriverFleetDashboardAnalyticsAllTime a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  (fleetOwnerId, requestorId) <- SharedLogic.Fleet.getFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a2) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a2) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDashboardAnalyticsAllTime a4 a3 fleetOwnerId (Kernel.Prelude.Just requestorId)

getDriverFleetDashboardAnalytics :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.FleetAnalyticsResponseType) -> Data.Time.Day -> Data.Time.Day -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetAnalyticsRes)
getDriverFleetDashboardAnalytics a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  (fleetOwnerId, requestorId) <- SharedLogic.Fleet.getFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a5) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a5) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5) a4
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDashboardAnalytics a7 a6 fleetOwnerId (Kernel.Prelude.Just requestorId) a3 a2 a1

postDriverFleetDashboardAnalyticsCache :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.FleetDashboardAnalyticsCacheReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetDashboardAnalyticsCache a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetDashboardAnalyticsCache a4 a3 a1

postDriverDashboardFleetEstimateRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.EstimateRouteReq -> Environment.FlowHandler Kernel.External.Maps.GetRoutesResp)
postDriverDashboardFleetEstimateRoute a5 a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.postDriverDashboardFleetEstimateRoute a5 a4 fleetOwnerId a1

postDriverFleetApproveDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.ApproveDriverReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetApproveDriver a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetApproveDriver a4 a3 fleetOwnerId a1

postDriverFleetDriverUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.UpdateDriverReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetDriverUpdate a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Kernel.Prelude.whenJust a1.email $ \email -> Tools.Auth.DashboardRegistration.assertDashboardEmailAvailable email a2.getId
  res <- Domain.Action.Dashboard.Fleet.Driver.postDriverFleetDriverUpdate a5 a4 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1
  Tools.Auth.DashboardRegistration.updateDashboardPersonProfile a2.getId a1.firstName a1.lastName a1.email a1.mobileNo a1.mobileCountryCode
  Kernel.Prelude.pure res

postDriverFleetDriverChangeFleetOwner :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.ChangeFleetOwnerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetDriverChangeFleetOwner a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetDriverChangeFleetOwner a5 a4 a2 a1

postDriverFleetVehicleChangeFleetOwner :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Driver.ChangeFleetOwnerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetVehicleChangeFleetOwner a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetVehicleChangeFleetOwner a5 a4 a2 a1

getDriverFleetDriverDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DriverDetailsRes)
getDriverFleetDriverDetails a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverDetails a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

postDriverFleetTripTransactionsV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.TripStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.TripTransactionRespT)
postDriverFleetTripTransactionsV2 a13 a12 _a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetTripTransactionsV2 a13 a12 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetVehicleListStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Data.Time.Day -> Data.Time.Day -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetVehicleStatsRes)
getDriverFleetVehicleListStats a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  (fleetOwnerId, requestorId) <- SharedLogic.Fleet.getFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a7) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a7) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a7) a6
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetVehicleListStats a9 a8 fleetOwnerId (Kernel.Prelude.Just requestorId) a5 a4 a3 a2 a1

getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.OnboardedDriversAndUnlinkedVehiclesRes)
getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) (Kernel.Prelude.Just a3)
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles a6 a5 fleetOwnerId a2 a1

getDriverFleetStatusSummary :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.EntityOperationType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.StatusSummaryResponse)
getDriverFleetStatusSummary a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetStatusSummary a5 a4 a2 a1

getDriverFleetScheduledBookingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Dashboard.Common.TripCategory) -> Kernel.Prelude.Maybe (Kernel.External.Maps.Types.LatLong) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetScheduledBookingListRes)
getDriverFleetScheduledBookingList a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverFleetScheduledBookingList a9 a8 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a7) a6 a5 a4 a3 a2 a1

postDriverFleetScheduledBookingAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.AssignScheduledBookingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetScheduledBookingAssign a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetScheduledBookingAssign a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

postDriverFleetScheduledBookingCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.CancelScheduledBookingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetScheduledBookingCancel a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetScheduledBookingCancel a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

postDriverFleetScheduledBookingReassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.ReassignScheduledBookingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetScheduledBookingReassign a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetScheduledBookingReassign a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

getDriverVehicleInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.VehicleInfo)
getDriverVehicleInfo a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.getDriverVehicleInfo a5 a4 a2 a1

postDriverFleetCashRideUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.UpdateCashRideReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetCashRideUpdate a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Driver.postDriverFleetCashRideUpdate a5 a4 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2 a1
