{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.Pass
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Pass
import qualified "this" API.Types.UI.Pass
import qualified Data.Time
import qualified Domain.Action.Dashboard.AppManagement.Pass
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Pass
import qualified "this" Domain.Types.PassType
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.PurchasedPass
import qualified "this" Domain.Types.PurchasedPassPayment
import qualified Environment
import EulerHS.Prelude
import qualified "shared-services" IssueManagement.Common.UI.Issue
import qualified "shared-services" IssueManagement.Domain.Types.MediaFile
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.Domain.Action
import qualified "payment" Lib.Payment.Domain.Types.PaymentOrder
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("pass" :> (GetPassCustomerAvailablePasses :<|> GetPassCustomerPurchasedPasses :<|> GetPassCustomerTransactions :<|> PostPassCustomerActivateToday :<|> PostPassCustomerPassSelect :<|> GetPassCustomerPaymentStatus :<|> PostPassCustomerPassResetDeviceSwitchCount :<|> PostPassCustomerPassUpdateProfilePicture :<|> GetPassCustomerPassPhoto :<|> PostPassCustomerPassRestore :<|> ListPassCatalog :<|> CreatePass :<|> UpdatePass :<|> DeletePass))

type GetPassCustomerAvailablePasses =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS/GET_PASS_CUSTOMER_AVAILABLE_PASSES"
      :> API.Types.Dashboard.AppManagement.Pass.GetPassCustomerAvailablePasses
  )

type GetPassCustomerPurchasedPasses =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS/GET_PASS_CUSTOMER_PURCHASED_PASSES"
      :> API.Types.Dashboard.AppManagement.Pass.GetPassCustomerPurchasedPasses
  )

type GetPassCustomerTransactions =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS/GET_PASS_CUSTOMER_TRANSACTIONS"
      :> API.Types.Dashboard.AppManagement.Pass.GetPassCustomerTransactions
  )

type PostPassCustomerActivateToday =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_ACTIVATE_TODAY"
      :> API.Types.Dashboard.AppManagement.Pass.PostPassCustomerActivateToday
  )

type PostPassCustomerPassSelect =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_SELECT"
      :> API.Types.Dashboard.AppManagement.Pass.PostPassCustomerPassSelect
  )

type GetPassCustomerPaymentStatus =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS/GET_PASS_CUSTOMER_PAYMENT_STATUS"
      :> API.Types.Dashboard.AppManagement.Pass.GetPassCustomerPaymentStatus
  )

type PostPassCustomerPassResetDeviceSwitchCount =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_RESET_DEVICE_SWITCH_COUNT"
      :> API.Types.Dashboard.AppManagement.Pass.PostPassCustomerPassResetDeviceSwitchCount
  )

type PostPassCustomerPassUpdateProfilePicture =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_UPDATE_PROFILE_PICTURE"
      :> API.Types.Dashboard.AppManagement.Pass.PostPassCustomerPassUpdateProfilePicture
  )

type GetPassCustomerPassPhoto =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS/GET_PASS_CUSTOMER_PASS_PHOTO"
      :> API.Types.Dashboard.AppManagement.Pass.GetPassCustomerPassPhoto
  )

type PostPassCustomerPassRestore =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_RESTORE"
      :> API.Types.Dashboard.AppManagement.Pass.PostPassCustomerPassRestore
  )

type ListPassCatalog = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/PASS/LIST_PASS_CATALOG" :> API.Types.Dashboard.AppManagement.Pass.ListPassCatalog)

type CreatePass = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/PASS/CREATE_PASS" :> API.Types.Dashboard.AppManagement.Pass.CreatePass)

type UpdatePass = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS" :> API.Types.Dashboard.AppManagement.Pass.UpdatePass)

type DeletePass = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/PASS/DELETE_PASS" :> API.Types.Dashboard.AppManagement.Pass.DeletePass)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPassCustomerAvailablePasses merchantId city :<|> getPassCustomerPurchasedPasses merchantId city :<|> getPassCustomerTransactions merchantId city :<|> postPassCustomerActivateToday merchantId city :<|> postPassCustomerPassSelect merchantId city :<|> getPassCustomerPaymentStatus merchantId city :<|> postPassCustomerPassResetDeviceSwitchCount merchantId city :<|> postPassCustomerPassUpdateProfilePicture merchantId city :<|> getPassCustomerPassPhoto merchantId city :<|> postPassCustomerPassRestore merchantId city :<|> listPassCatalog merchantId city :<|> createPass merchantId city :<|> updatePass merchantId city :<|> deletePass merchantId city

getPassCustomerAvailablePasses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Environment.FlowHandler [API.Types.UI.Pass.PassInfoAPIEntity])
getPassCustomerAvailablePasses a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerAvailablePasses a5 a4 a2 a1

getPassCustomerPurchasedPasses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Kernel.Prelude.Maybe (Domain.Types.PurchasedPass.StatusType) -> Environment.FlowHandler [API.Types.UI.Pass.PurchasedPassAPIEntity])
getPassCustomerPurchasedPasses a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerPurchasedPasses a6 a5 a3 a2 a1

getPassCustomerTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler [API.Types.UI.Pass.PurchasedPassTransactionAPIEntity])
getPassCustomerTransactions a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerTransactions a7 a6 a4 a3 a2 a1

postPassCustomerActivateToday :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassCustomerActivateToday a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerActivateToday a7 a6 a4 a3 a2 a1

postPassCustomerPassSelect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> API.Types.Dashboard.AppManagement.Pass.PurchasedPassSelectReq -> Environment.FlowHandler API.Types.UI.Pass.PassSelectionAPIEntity)
postPassCustomerPassSelect a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerPassSelect a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a1

getPassCustomerPaymentStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> Environment.FlowHandler Lib.Payment.Domain.Action.PaymentStatusResp)
getPassCustomerPaymentStatus a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerPaymentStatus a5 a4 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3))

postPassCustomerPassResetDeviceSwitchCount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassCustomerPassResetDeviceSwitchCount a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerPassResetDeviceSwitchCount a5 a4 a2 a1

postPassCustomerPassUpdateProfilePicture :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> IssueManagement.Common.UI.Issue.IssueMediaUploadReq -> Environment.FlowHandler IssueManagement.Common.UI.Issue.IssueMediaUploadRes)
postPassCustomerPassUpdateProfilePicture a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerPassUpdateProfilePicture a6 a5 a3 a2 a1

getPassCustomerPassPhoto :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> Environment.FlowHandler Kernel.Prelude.Text)
getPassCustomerPassPhoto a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerPassPhoto a5 a4 a2 a1

postPassCustomerPassRestore :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassCustomerPassRestore a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerPassRestore a4 a3 a1

listPassCatalog :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassType.PassType) -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.Pass.PassCatalogItem])
listPassCatalog a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.listPassCatalog a5 a4 a2 a1

createPass :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.Pass.PassCreateReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Pass.PassCreateResp)
createPass a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.createPass a4 a3 a1

updatePass :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> API.Types.Dashboard.AppManagement.Pass.PassUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
updatePass a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.updatePass a5 a4 a2 a1

deletePass :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deletePass a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.deletePass a4 a3 a1
