{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Customer
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Customer
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Customer
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

type API = ("customer" :> (GetCustomerList :<|> DeleteCustomerDelete :<|> PostCustomerBlock :<|> PostCustomerUnblock :<|> GetCustomerInfo :<|> GetCustomerCancellationDuesDetails :<|> PostCustomerUpdateSafetyCenterBlocking :<|> PostCustomerPersonNumbers :<|> PostCustomerPersonId :<|> PostCustomerUpdatePaymentMode :<|> PostCustomerOffersList :<|> PostCustomerApplyOffer :<|> PostCustomerEnsureExists :<|> PostCustomerBulkApplyOffer))

type GetCustomerList = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/CUSTOMER/GET_CUSTOMER_LIST" :> API.Types.RiderPlatform.Management.Customer.GetCustomerList)

type DeleteCustomerDelete = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/CUSTOMER/DELETE_CUSTOMER_DELETE" :> API.Types.RiderPlatform.Management.Customer.DeleteCustomerDelete)

type PostCustomerBlock = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_BLOCK" :> API.Types.RiderPlatform.Management.Customer.PostCustomerBlock)

type PostCustomerUnblock = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_UNBLOCK" :> API.Types.RiderPlatform.Management.Customer.PostCustomerUnblock)

type GetCustomerInfo = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/CUSTOMER/GET_CUSTOMER_INFO" :> API.Types.RiderPlatform.Management.Customer.GetCustomerInfo)

type GetCustomerCancellationDuesDetails =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/CUSTOMER/GET_CUSTOMER_CANCELLATION_DUES_DETAILS"
      :> API.Types.RiderPlatform.Management.Customer.GetCustomerCancellationDuesDetails
  )

type PostCustomerUpdateSafetyCenterBlocking =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_UPDATE_SAFETY_CENTER_BLOCKING"
      :> API.Types.RiderPlatform.Management.Customer.PostCustomerUpdateSafetyCenterBlocking
  )

type PostCustomerPersonNumbers =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_PERSON_NUMBERS"
      :> API.Types.RiderPlatform.Management.Customer.PostCustomerPersonNumbers
  )

type PostCustomerPersonId = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_PERSON_ID" :> API.Types.RiderPlatform.Management.Customer.PostCustomerPersonId)

type PostCustomerUpdatePaymentMode =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_UPDATE_PAYMENT_MODE"
      :> API.Types.RiderPlatform.Management.Customer.PostCustomerUpdatePaymentMode
  )

type PostCustomerOffersList = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_OFFERS_LIST" :> API.Types.RiderPlatform.Management.Customer.PostCustomerOffersList)

type PostCustomerApplyOffer = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_APPLY_OFFER" :> API.Types.RiderPlatform.Management.Customer.PostCustomerApplyOffer)

type PostCustomerEnsureExists =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_ENSURE_EXISTS"
      :> API.Types.RiderPlatform.Management.Customer.PostCustomerEnsureExists
  )

type PostCustomerBulkApplyOffer =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_BULK_APPLY_OFFER"
      :> API.Types.RiderPlatform.Management.Customer.PostCustomerBulkApplyOffer
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getCustomerList merchantId city :<|> deleteCustomerDelete merchantId city :<|> postCustomerBlock merchantId city :<|> postCustomerUnblock merchantId city :<|> getCustomerInfo merchantId city :<|> getCustomerCancellationDuesDetails merchantId city :<|> postCustomerUpdateSafetyCenterBlocking merchantId city :<|> postCustomerPersonNumbers merchantId city :<|> postCustomerPersonId merchantId city :<|> postCustomerUpdatePaymentMode merchantId city :<|> postCustomerOffersList merchantId city :<|> postCustomerApplyOffer merchantId city :<|> postCustomerEnsureExists merchantId city :<|> postCustomerBulkApplyOffer merchantId city

getCustomerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Customer) -> Environment.FlowHandler API.Types.RiderPlatform.Management.Customer.CustomerListRes)
getCustomerList a10 a9 _a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.getCustomerList a10 a9 a7 a6 a5 a4 a3 a2 a1

deleteCustomerDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteCustomerDelete a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.deleteCustomerDelete a4 a3 a1

postCustomerBlock :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Customer -> API.Types.RiderPlatform.Management.Customer.BlockCustomerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerBlock a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerBlock a5 a4 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorName a3) a1

postCustomerUnblock :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerUnblock a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerUnblock a4 a3 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorName a2)

getCustomerInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler API.Types.RiderPlatform.Management.Customer.CustomerInfoRes)
getCustomerInfo a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.getCustomerInfo a4 a3 a1

getCustomerCancellationDuesDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler API.Types.RiderPlatform.Management.Customer.CancellationDuesDetailsRes)
getCustomerCancellationDuesDetails a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.getCustomerCancellationDuesDetails a4 a3 a1

postCustomerUpdateSafetyCenterBlocking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Customer -> API.Types.RiderPlatform.Management.Customer.UpdateSafetyCenterBlockingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerUpdateSafetyCenterBlocking a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerUpdateSafetyCenterBlocking a5 a4 a2 a1

postCustomerPersonNumbers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.PersonIdsReq -> Environment.FlowHandler [Dashboard.Common.PersonRes])
postCustomerPersonNumbers a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerPersonNumbers a4 a3 a1

postCustomerPersonId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.PersonMobileNoReq -> Environment.FlowHandler [Dashboard.Common.PersonRes])
postCustomerPersonId a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerPersonId a4 a3 a1

postCustomerUpdatePaymentMode :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Customer -> API.Types.RiderPlatform.Management.Customer.UpdatePaymentModeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerUpdatePaymentMode a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerUpdatePaymentMode a5 a4 a2 a1

postCustomerOffersList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Customer.CustomerOffersListReq -> Environment.FlowHandler [API.Types.RiderPlatform.Management.Customer.CustomerOfferEntity])
postCustomerOffersList a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerOffersList a4 a3 a1

postCustomerApplyOffer :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Customer.ApplyCustomerOfferReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerApplyOffer a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerApplyOffer a4 a3 a1

postCustomerEnsureExists :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Customer.CustomerEnsureExistsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerEnsureExists a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerEnsureExists a4 a3 a1

postCustomerBulkApplyOffer :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Customer.BulkApplyCustomerOfferReq -> Environment.FlowHandler [API.Types.RiderPlatform.Management.Customer.BulkApplyCustomerOfferRes])
postCustomerBulkApplyOffer a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerBulkApplyOffer a4 a3 a1
