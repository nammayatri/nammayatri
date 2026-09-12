{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking.Driver
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Driver
import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Domain.Action.Dashboard.RideBooking.Driver
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("driver" :> (GetDriverPaymentDue :<|> PostDriverEnable :<|> PostDriverCollectCashHelper :<|> PostDriverV2CollectCash :<|> PostDriverExemptCashHelper :<|> PostDriverV2ExemptCash :<|> GetDriverInfo :<|> GetDriverFeedbackList :<|> PostDriverUnlinkVehicle :<|> PostDriverEndRCAssociation :<|> PostDriverDeleteAadhaar :<|> PostDriverDeletePanCard :<|> PostDriverAddVehicle :<|> PostDriverSetRCStatus :<|> PostDriverExemptDriverFee))

type GetDriverPaymentDue = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/DRIVER/GET_DRIVER_PAYMENT_DUE" :> API.Types.Dashboard.RideBooking.Driver.GetDriverPaymentDue)

type PostDriverEnable = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_ENABLE" :> API.Types.Dashboard.RideBooking.Driver.PostDriverEnable)

type PostDriverCollectCashHelper = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_COLLECT_CASH" :> API.Types.Dashboard.RideBooking.Driver.PostDriverCollectCashHelper)

type PostDriverV2CollectCash =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP)
      "PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_V2_COLLECT_CASH"
      :> API.Types.Dashboard.RideBooking.Driver.PostDriverV2CollectCash
  )

type PostDriverExemptCashHelper = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_EXEMPT_CASH" :> API.Types.Dashboard.RideBooking.Driver.PostDriverExemptCashHelper)

type PostDriverV2ExemptCash =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP)
      "PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_V2_EXEMPT_CASH"
      :> API.Types.Dashboard.RideBooking.Driver.PostDriverV2ExemptCash
  )

-- Public shape: the Helper's fleetOwnerId/mbFleet come from the session.
type GetDriverInfo = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/DRIVER/GET_DRIVER_INFO" :> API.Types.Dashboard.RideBooking.Driver.GetDriverInfo)

type GetDriverFeedbackList = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/DRIVER/GET_DRIVER_FEEDBACK_LIST" :> API.Types.Dashboard.RideBooking.Driver.GetDriverFeedbackList)

type PostDriverUnlinkVehicle = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_UNLINK_VEHICLE" :> API.Types.Dashboard.RideBooking.Driver.PostDriverUnlinkVehicle)

type PostDriverEndRCAssociation =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP)
      "PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_END_RC_ASSOCIATION"
      :> API.Types.Dashboard.RideBooking.Driver.PostDriverEndRCAssociation
  )

type PostDriverDeleteAadhaar = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_DELETE_AADHAAR" :> API.Types.Dashboard.RideBooking.Driver.PostDriverDeleteAadhaar)

type PostDriverDeletePanCard = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_DELETE_PAN_CARD" :> API.Types.Dashboard.RideBooking.Driver.PostDriverDeletePanCard)

type PostDriverAddVehicle = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_ADD_VEHICLE" :> API.Types.Dashboard.RideBooking.Driver.PostDriverAddVehicle)

type PostDriverSetRCStatus = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_SET_RC_STATUS" :> API.Types.Dashboard.RideBooking.Driver.PostDriverSetRCStatus)

type PostDriverExemptDriverFee =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP)
      "PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_EXEMPT_DRIVER_FEE"
      :> API.Types.Dashboard.RideBooking.Driver.PostDriverExemptDriverFee
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverPaymentDue merchantId city :<|> postDriverEnable merchantId city :<|> postDriverCollectCash merchantId city :<|> postDriverV2CollectCash merchantId city :<|> postDriverExemptCash merchantId city :<|> postDriverV2ExemptCash merchantId city :<|> getDriverInfo merchantId city :<|> getDriverFeedbackList merchantId city :<|> postDriverUnlinkVehicle merchantId city :<|> postDriverEndRCAssociation merchantId city :<|> postDriverDeleteAadhaar merchantId city :<|> postDriverDeletePanCard merchantId city :<|> postDriverAddVehicle merchantId city :<|> postDriverSetRCStatus merchantId city :<|> postDriverExemptDriverFee merchantId city

getDriverPaymentDue :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Text -> Environment.FlowHandler [API.Types.Dashboard.RideBooking.Driver.DriverOutstandingBalanceResp])
getDriverPaymentDue a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Driver.getDriverPaymentDue a5 a4 a2 a1

postDriverEnable :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverEnable a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Driver.postDriverEnable a4 a3 a1

postDriverCollectCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverCollectCash a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Driver.postDriverCollectCash a6 a5 a3 a2 a1

postDriverV2CollectCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Dashboard.Common.Driver.ServiceNames -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverV2CollectCash a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Driver.postDriverV2CollectCash a6 a5 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) a2 a1

postDriverExemptCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverExemptCash a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Driver.postDriverExemptCash a5 a4 a2 a1

postDriverV2ExemptCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Dashboard.Common.Driver.ServiceNames -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverV2ExemptCash a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Driver.postDriverV2ExemptCash a5 a4 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1

getDriverInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.Dashboard.RideBooking.Driver.DriverInfoRes)
getDriverInfo a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  mbFleet <- Tools.Auth.DashboardUserAuth.requestorFleetFlag a9
  Domain.Action.Dashboard.RideBooking.Driver.getDriverInfo a11 a10 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a9) mbFleet a8 a7 a6 a5 a4 a3 a2 a1

getDriverFeedbackList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.Dashboard.RideBooking.Driver.GetFeedbackListRes)
getDriverFeedbackList a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Driver.getDriverFeedbackList a6 a5 a3 a2 a1

postDriverUnlinkVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUnlinkVehicle a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Driver.postDriverUnlinkVehicle a4 a3 a1

postDriverEndRCAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverEndRCAssociation a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Driver.postDriverEndRCAssociation a4 a3 a1

postDriverDeleteAadhaar :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverDeleteAadhaar a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Driver.postDriverDeleteAadhaar a4 a3 a1

postDriverDeletePanCard :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverDeletePanCard a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Driver.postDriverDeletePanCard a4 a3 a1

postDriverAddVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.AddVehicleReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverAddVehicle a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Driver.postDriverAddVehicle a5 a4 a2 a1

postDriverSetRCStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.RCStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverSetRCStatus a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Driver.postDriverSetRCStatus a5 a4 a2 a1

postDriverExemptDriverFee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Dashboard.Common.Driver.ServiceNames -> API.Types.Dashboard.RideBooking.Driver.ExemptionAndCashCollectionDriverFeeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverExemptDriverFee a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Driver.postDriverExemptDriverFee a6 a5 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) a2 a1
