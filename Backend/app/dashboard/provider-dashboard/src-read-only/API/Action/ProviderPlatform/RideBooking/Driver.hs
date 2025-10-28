{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.RideBooking.Driver
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Driver
import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Domain.Action.ProviderPlatform.RideBooking.Driver
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("driver" :> (GetDriverPaymentDue :<|> PostDriverEnable :<|> PostDriverCollectCash :<|> PostDriverV2CollectCash :<|> PostDriverExemptCash :<|> PostDriverV2ExemptCash :<|> GetDriverInfo :<|> GetDriverFeedbackList :<|> PostDriverUnlinkVehicle :<|> PostDriverEndRCAssociation :<|> PostDriverAddVehicle :<|> PostDriverSetRCStatus :<|> PostDriverExemptDriverFee))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverPaymentDue merchantId city :<|> postDriverEnable merchantId city :<|> postDriverCollectCash merchantId city :<|> postDriverV2CollectCash merchantId city :<|> postDriverExemptCash merchantId city :<|> postDriverV2ExemptCash merchantId city :<|> getDriverInfo merchantId city :<|> getDriverFeedbackList merchantId city :<|> postDriverUnlinkVehicle merchantId city :<|> postDriverEndRCAssociation merchantId city :<|> postDriverAddVehicle merchantId city :<|> postDriverSetRCStatus merchantId city :<|> postDriverExemptDriverFee merchantId city

type GetDriverPaymentDue =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER / 'API.Types.Dashboard.RideBooking.Driver.GET_DRIVER_PAYMENT_DUE)
      :> API.Types.Dashboard.RideBooking.Driver.GetDriverPaymentDue
  )

type PostDriverEnable =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER / 'API.Types.Dashboard.RideBooking.Driver.POST_DRIVER_ENABLE)
      :> API.Types.Dashboard.RideBooking.Driver.PostDriverEnable
  )

type PostDriverCollectCash =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER / 'API.Types.Dashboard.RideBooking.Driver.POST_DRIVER_COLLECT_CASH)
      :> API.Types.Dashboard.RideBooking.Driver.PostDriverCollectCash
  )

type PostDriverV2CollectCash =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER / 'API.Types.Dashboard.RideBooking.Driver.POST_DRIVER_V2_COLLECT_CASH)
      :> API.Types.Dashboard.RideBooking.Driver.PostDriverV2CollectCash
  )

type PostDriverExemptCash =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER / 'API.Types.Dashboard.RideBooking.Driver.POST_DRIVER_EXEMPT_CASH)
      :> API.Types.Dashboard.RideBooking.Driver.PostDriverExemptCash
  )

type PostDriverV2ExemptCash =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER / 'API.Types.Dashboard.RideBooking.Driver.POST_DRIVER_V2_EXEMPT_CASH)
      :> API.Types.Dashboard.RideBooking.Driver.PostDriverV2ExemptCash
  )

type GetDriverInfo =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER / 'API.Types.Dashboard.RideBooking.Driver.GET_DRIVER_INFO)
      :> API.Types.Dashboard.RideBooking.Driver.GetDriverInfo
  )

type GetDriverFeedbackList =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER / 'API.Types.Dashboard.RideBooking.Driver.GET_DRIVER_FEEDBACK_LIST)
      :> API.Types.Dashboard.RideBooking.Driver.GetDriverFeedbackList
  )

type PostDriverUnlinkVehicle =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER / 'API.Types.Dashboard.RideBooking.Driver.POST_DRIVER_UNLINK_VEHICLE)
      :> API.Types.Dashboard.RideBooking.Driver.PostDriverUnlinkVehicle
  )

type PostDriverEndRCAssociation =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER / 'API.Types.Dashboard.RideBooking.Driver.POST_DRIVER_END_RC_ASSOCIATION)
      :> API.Types.Dashboard.RideBooking.Driver.PostDriverEndRCAssociation
  )

type PostDriverAddVehicle =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER / 'API.Types.Dashboard.RideBooking.Driver.POST_DRIVER_ADD_VEHICLE)
      :> API.Types.Dashboard.RideBooking.Driver.PostDriverAddVehicle
  )

type PostDriverSetRCStatus =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER / 'API.Types.Dashboard.RideBooking.Driver.POST_DRIVER_SET_RC_STATUS)
      :> API.Types.Dashboard.RideBooking.Driver.PostDriverSetRCStatus
  )

type PostDriverExemptDriverFee =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.DRIVER / 'API.Types.Dashboard.RideBooking.Driver.POST_DRIVER_EXEMPT_DRIVER_FEE)
      :> API.Types.Dashboard.RideBooking.Driver.PostDriverExemptDriverFee
  )

getDriverPaymentDue :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler [API.Types.Dashboard.RideBooking.Driver.DriverOutstandingBalanceResp])
getDriverPaymentDue merchantShortId opCity apiTokenInfo countryCode phone = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.getDriverPaymentDue merchantShortId opCity apiTokenInfo countryCode phone

postDriverEnable :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverEnable merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverEnable merchantShortId opCity apiTokenInfo driverId

postDriverCollectCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverCollectCash merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverCollectCash merchantShortId opCity apiTokenInfo driverId

postDriverV2CollectCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Dashboard.Common.Driver.ServiceNames -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverV2CollectCash merchantShortId opCity apiTokenInfo driverId serviceName = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverV2CollectCash merchantShortId opCity apiTokenInfo driverId serviceName

postDriverExemptCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverExemptCash merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverExemptCash merchantShortId opCity apiTokenInfo driverId

postDriverV2ExemptCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Dashboard.Common.Driver.ServiceNames -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverV2ExemptCash merchantShortId opCity apiTokenInfo driverId serviceName = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverV2ExemptCash merchantShortId opCity apiTokenInfo driverId serviceName

getDriverInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Environment.FlowHandler API.Types.Dashboard.RideBooking.Driver.DriverInfoRes)
getDriverInfo merchantShortId opCity apiTokenInfo mobileNumber mobileCountryCode vehicleNumber dlNumber rcNumber email personId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.getDriverInfo merchantShortId opCity apiTokenInfo mobileNumber mobileCountryCode vehicleNumber dlNumber rcNumber email personId

getDriverFeedbackList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.RideBooking.Driver.GetFeedbackListRes)
getDriverFeedbackList merchantShortId opCity apiTokenInfo personId mobileNumber mobileCountryCode = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.getDriverFeedbackList merchantShortId opCity apiTokenInfo personId mobileNumber mobileCountryCode

postDriverUnlinkVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUnlinkVehicle merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverUnlinkVehicle merchantShortId opCity apiTokenInfo driverId

postDriverEndRCAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverEndRCAssociation merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverEndRCAssociation merchantShortId opCity apiTokenInfo driverId

postDriverAddVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.AddVehicleReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverAddVehicle merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverAddVehicle merchantShortId opCity apiTokenInfo driverId req

postDriverSetRCStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.RCStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverSetRCStatus merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverSetRCStatus merchantShortId opCity apiTokenInfo driverId req

postDriverExemptDriverFee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Dashboard.Common.Driver.ServiceNames -> API.Types.Dashboard.RideBooking.Driver.ExemptionAndCashCollectionDriverFeeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverExemptDriverFee merchantShortId opCity apiTokenInfo driverId serviceName req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverExemptDriverFee merchantShortId opCity apiTokenInfo driverId serviceName req
