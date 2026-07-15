{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.Pass
  ( API.Types.Dashboard.AppManagement.Pass.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Pass
import qualified "this" API.Types.UI.Pass
import qualified Data.Time
import qualified Domain.Action.Dashboard.AppManagement.Pass
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Pass
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.Pass.API)
handler merchantId city = getPassCustomerAvailablePasses merchantId city :<|> getPassCustomerPurchasedPasses merchantId city :<|> getPassCustomerTransactions merchantId city :<|> postPassCustomerActivateToday merchantId city :<|> postPassCustomerPassSelect merchantId city :<|> getPassCustomerPaymentStatus merchantId city :<|> postPassCustomerPassResetDeviceSwitchCount merchantId city :<|> postPassCustomerPassUpdateProfilePicture merchantId city :<|> getPassCustomerPassPhoto merchantId city :<|> postPassCustomerPassRestore merchantId city

getPassCustomerAvailablePasses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler [API.Types.UI.Pass.PassInfoAPIEntity])
getPassCustomerAvailablePasses a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerAvailablePasses a4 a3 a2 a1

getPassCustomerPurchasedPasses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Kernel.Prelude.Maybe Domain.Types.PurchasedPass.StatusType -> Environment.FlowHandler [API.Types.UI.Pass.PurchasedPassAPIEntity])
getPassCustomerPurchasedPasses a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerPurchasedPasses a5 a4 a3 a2 a1

getPassCustomerTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler [API.Types.UI.Pass.PurchasedPassTransactionAPIEntity])
getPassCustomerTransactions a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerTransactions a6 a5 a4 a3 a2 a1

postPassCustomerActivateToday :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment) -> Kernel.Prelude.Maybe Data.Time.Day -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassCustomerActivateToday a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerActivateToday a6 a5 a4 a3 a2 a1

postPassCustomerPassSelect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.Dashboard.AppManagement.Pass.PurchasedPassSelectReq -> Environment.FlowHandler API.Types.UI.Pass.PassSelectionAPIEntity)
postPassCustomerPassSelect a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerPassSelect a6 a5 a4 a3 a2 a1

getPassCustomerPaymentStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler Lib.Payment.Domain.Action.PaymentStatusResp)
getPassCustomerPaymentStatus a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerPaymentStatus a5 a4 a3 a2 a1

postPassCustomerPassResetDeviceSwitchCount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassCustomerPassResetDeviceSwitchCount a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerPassResetDeviceSwitchCount a4 a3 a2 a1

postPassCustomerPassUpdateProfilePicture :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> IssueManagement.Common.UI.Issue.IssueMediaUploadReq -> Environment.FlowHandler IssueManagement.Common.UI.Issue.IssueMediaUploadRes)
postPassCustomerPassUpdateProfilePicture a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerPassUpdateProfilePicture a5 a4 a3 a2 a1

getPassCustomerPassPhoto :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> Environment.FlowHandler Kernel.Prelude.Text)
getPassCustomerPassPhoto a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerPassPhoto a4 a3 a2 a1

postPassCustomerPassRestore :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassCustomerPassRestore a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerPassRestore a3 a2 a1
