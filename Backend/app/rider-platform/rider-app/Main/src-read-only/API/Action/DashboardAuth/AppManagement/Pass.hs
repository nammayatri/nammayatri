{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.Pass
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Pass
import qualified "this" API.Types.UI.Pass
import qualified Control.Lens
import qualified Data.Time
import qualified Domain.Action.Dashboard.AppManagement.Pass
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Pass
import qualified "this" Domain.Types.PassCategory
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
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("pass" :> (GetPassCustomerAvailablePasses :<|> GetPassCustomerPurchasedPasses :<|> GetPassCustomerTransactions :<|> PostPassCustomerActivateToday :<|> PostPassCustomerPassSelect :<|> GetPassCustomerPaymentStatus :<|> PostPassCustomerPassResetDeviceSwitchCount :<|> PostPassCustomerPassUpdateProfilePicture :<|> GetPassCustomerPassPhoto :<|> PostPassCustomerPassRestore :<|> ListPassCatalog :<|> CreatePass :<|> UpdatePass :<|> DeletePass :<|> GetPassCatalog :<|> ListPassType :<|> CreatePassType :<|> UpdatePassType :<|> ListPassCategory :<|> CreatePassCategory :<|> UpdatePassCategory :<|> GetPurchasedPassPaymentTrips :<|> AdjustPurchasedPassPaymentTrips))

type GetPassCustomerAvailablePasses =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/PASS/GET_PASS_CUSTOMER_AVAILABLE_PASSES"
      :> API.Types.Dashboard.AppManagement.Pass.GetPassCustomerAvailablePasses
  )

type GetPassCustomerPurchasedPasses =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/PASS/GET_PASS_CUSTOMER_PURCHASED_PASSES"
      :> API.Types.Dashboard.AppManagement.Pass.GetPassCustomerPurchasedPasses
  )

type GetPassCustomerTransactions =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/PASS/GET_PASS_CUSTOMER_TRANSACTIONS"
      :> API.Types.Dashboard.AppManagement.Pass.GetPassCustomerTransactions
  )

type PostPassCustomerActivateToday =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_ACTIVATE_TODAY"
      :> API.Types.Dashboard.AppManagement.Pass.PostPassCustomerActivateToday
  )

type PostPassCustomerPassSelect =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_SELECT"
      :> API.Types.Dashboard.AppManagement.Pass.PostPassCustomerPassSelect
  )

type GetPassCustomerPaymentStatus =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/PASS/GET_PASS_CUSTOMER_PAYMENT_STATUS"
      :> API.Types.Dashboard.AppManagement.Pass.GetPassCustomerPaymentStatus
  )

type PostPassCustomerPassResetDeviceSwitchCount =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_RESET_DEVICE_SWITCH_COUNT"
      :> API.Types.Dashboard.AppManagement.Pass.PostPassCustomerPassResetDeviceSwitchCount
  )

type PostPassCustomerPassUpdateProfilePicture =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_UPDATE_PROFILE_PICTURE"
      :> API.Types.Dashboard.AppManagement.Pass.PostPassCustomerPassUpdateProfilePicture
  )

type GetPassCustomerPassPhoto =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/PASS/GET_PASS_CUSTOMER_PASS_PHOTO"
      :> API.Types.Dashboard.AppManagement.Pass.GetPassCustomerPassPhoto
  )

type PostPassCustomerPassRestore =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_RESTORE"
      :> API.Types.Dashboard.AppManagement.Pass.PostPassCustomerPassRestore
  )

type ListPassCatalog = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/LIST_PASS_CATALOG" :> API.Types.Dashboard.AppManagement.Pass.ListPassCatalog)

type CreatePass = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/CREATE_PASS" :> API.Types.Dashboard.AppManagement.Pass.CreatePass)

type UpdatePass = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS" :> API.Types.Dashboard.AppManagement.Pass.UpdatePass)

type DeletePass = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/DELETE_PASS" :> API.Types.Dashboard.AppManagement.Pass.DeletePass)

type GetPassCatalog = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/GET_PASS_CATALOG" :> API.Types.Dashboard.AppManagement.Pass.GetPassCatalog)

type ListPassType = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/LIST_PASS_TYPE" :> API.Types.Dashboard.AppManagement.Pass.ListPassType)

type CreatePassType = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/CREATE_PASS_TYPE" :> API.Types.Dashboard.AppManagement.Pass.CreatePassType)

type UpdatePassType = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS_TYPE" :> API.Types.Dashboard.AppManagement.Pass.UpdatePassType)

type ListPassCategory = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/LIST_PASS_CATEGORY" :> API.Types.Dashboard.AppManagement.Pass.ListPassCategory)

type CreatePassCategory = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/CREATE_PASS_CATEGORY" :> API.Types.Dashboard.AppManagement.Pass.CreatePassCategory)

type UpdatePassCategory = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS_CATEGORY" :> API.Types.Dashboard.AppManagement.Pass.UpdatePassCategory)

type GetPurchasedPassPaymentTrips =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/PASS/GET_PURCHASED_PASS_PAYMENT_TRIPS"
      :> API.Types.Dashboard.AppManagement.Pass.GetPurchasedPassPaymentTrips
  )

type AdjustPurchasedPassPaymentTrips =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/PASS/ADJUST_PURCHASED_PASS_PAYMENT_TRIPS"
      :> API.Types.Dashboard.AppManagement.Pass.AdjustPurchasedPassPaymentTrips
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPassCustomerAvailablePasses merchantId city :<|> getPassCustomerPurchasedPasses merchantId city :<|> getPassCustomerTransactions merchantId city :<|> postPassCustomerActivateToday merchantId city :<|> postPassCustomerPassSelect merchantId city :<|> getPassCustomerPaymentStatus merchantId city :<|> postPassCustomerPassResetDeviceSwitchCount merchantId city :<|> postPassCustomerPassUpdateProfilePicture merchantId city :<|> getPassCustomerPassPhoto merchantId city :<|> postPassCustomerPassRestore merchantId city :<|> listPassCatalog merchantId city :<|> createPass merchantId city :<|> updatePass merchantId city :<|> deletePass merchantId city :<|> getPassCatalog merchantId city :<|> listPassType merchantId city :<|> createPassType merchantId city :<|> updatePassType merchantId city :<|> listPassCategory merchantId city :<|> createPassCategory merchantId city :<|> updatePassCategory merchantId city :<|> getPurchasedPassPaymentTrips merchantId city :<|> adjustPurchasedPassPaymentTrips merchantId city

getPassCustomerAvailablePasses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler [API.Types.UI.Pass.PassInfoAPIEntity])
getPassCustomerAvailablePasses a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerAvailablePasses a5 a4 a2 a1

getPassCustomerPurchasedPasses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Kernel.Prelude.Maybe Domain.Types.PurchasedPass.StatusType -> Environment.FlowHandler [API.Types.UI.Pass.PurchasedPassAPIEntity])
getPassCustomerPurchasedPasses a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerPurchasedPasses a6 a5 a3 a2 a1

getPassCustomerTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler [API.Types.UI.Pass.PurchasedPassTransactionAPIEntity])
getPassCustomerTransactions a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerTransactions a7 a6 a4 a3 a2 a1

postPassCustomerActivateToday :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment) -> Kernel.Prelude.Maybe Data.Time.Day -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassCustomerActivateToday a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_ACTIVATE_TODAY" a5 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerActivateToday a7 a6 a4 a3 a2 a1
    )

postPassCustomerPassSelect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> API.Types.Dashboard.AppManagement.Pass.PurchasedPassSelectReq -> Environment.FlowHandler API.Types.UI.Pass.PassSelectionAPIEntity)
postPassCustomerPassSelect a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_SELECT" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerPassSelect a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a1
    )

getPassCustomerPaymentStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> Environment.FlowHandler Lib.Payment.Domain.Action.PaymentStatusResp)
getPassCustomerPaymentStatus a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerPaymentStatus a5 a4 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3))

postPassCustomerPassResetDeviceSwitchCount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassCustomerPassResetDeviceSwitchCount a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_RESET_DEVICE_SWITCH_COUNT" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerPassResetDeviceSwitchCount a5 a4 a2 a1
    )

postPassCustomerPassUpdateProfilePicture :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> IssueManagement.Common.UI.Issue.IssueMediaUploadReq -> Environment.FlowHandler IssueManagement.Common.UI.Issue.IssueMediaUploadRes)
postPassCustomerPassUpdateProfilePicture a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_UPDATE_PROFILE_PICTURE" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerPassUpdateProfilePicture a6 a5 a3 a2 a1
    )

getPassCustomerPassPhoto :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> Environment.FlowHandler Kernel.Prelude.Text)
getPassCustomerPassPhoto a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerPassPhoto a5 a4 a2 a1

postPassCustomerPassRestore :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassCustomerPassRestore a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_RESTORE" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerPassRestore a4 a3 a1
    )

listPassCatalog :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassType.PassType) -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.Pass.PassCatalogItem])
listPassCatalog a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.Pass.listPassCatalog a5 a4 a2 a1

createPass :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.Pass.PassCreateReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Pass.PassCreateResp)
createPass a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/CREATE_PASS" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.Pass.createPass a4 a3 a1
    )

updatePass :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> API.Types.Dashboard.AppManagement.Pass.PassUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
updatePass a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.Pass.updatePass a5 a4 a2 a1
    )

deletePass :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deletePass a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/DELETE_PASS" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.Pass.deletePass a4 a3 a1
    )

getPassCatalog :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Pass.PassCatalogItem)
getPassCatalog a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.Pass.getPassCatalog a4 a3 a1

listPassType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory) -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.Pass.PassTypeItem])
listPassType a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.Pass.listPassType a5 a4 a2 a1

createPassType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.Pass.PassTypeCreateReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Pass.PassTypeCreateResp)
createPassType a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/CREATE_PASS_TYPE" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.Pass.createPassType a4 a3 a1
    )

updatePassType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.PassType.PassType -> API.Types.Dashboard.AppManagement.Pass.PassTypeUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
updatePassType a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS_TYPE" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.Pass.updatePassType a5 a4 a2 a1
    )

listPassCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.Pass.PassCategoryItem])
listPassCategory a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.Pass.listPassCategory a4 a3 a1

createPassCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.Pass.PassCategoryCreateReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Pass.PassCategoryCreateResp)
createPassCategory a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/CREATE_PASS_CATEGORY" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.Pass.createPassCategory a4 a3 a1
    )

updatePassCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory -> API.Types.Dashboard.AppManagement.Pass.PassCategoryUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
updatePassCategory a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS_CATEGORY" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.Pass.updatePassCategory a5 a4 a2 a1
    )

getPurchasedPassPaymentTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Pass.PurchasedPassPaymentTripsResp)
getPurchasedPassPaymentTrips a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.Pass.getPurchasedPassPaymentTrips a4 a3 a1

adjustPurchasedPassPaymentTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> API.Types.Dashboard.AppManagement.Pass.PurchasedPassPaymentTripsAdjustReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Pass.PurchasedPassPaymentTripsResp)
adjustPurchasedPassPaymentTrips a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/PASS/ADJUST_PURCHASED_PASS_PAYMENT_TRIPS" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.Pass.adjustPurchasedPassPaymentTrips a5 a4 a2 a1
    )
