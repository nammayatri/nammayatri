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
import qualified Environment
import EulerHS.Prelude
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
handler merchantId city = getPassCustomerAvailablePasses merchantId city :<|> getPassCustomerPurchasedPasses merchantId city :<|> getPassCustomerTransactions merchantId city :<|> postPassCustomerActivateToday merchantId city :<|> postPassCustomerPassSelect merchantId city :<|> getPassCustomerPaymentStatus merchantId city :<|> postPassCustomerPassResetDeviceSwitchCount merchantId city :<|> postPassCustomerPassUpdateProfilePicture merchantId city

getPassCustomerAvailablePasses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Environment.FlowHandler [API.Types.UI.Pass.PassInfoAPIEntity])
getPassCustomerAvailablePasses a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerAvailablePasses a4 a3 a2 a1

getPassCustomerPurchasedPasses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Kernel.Prelude.Maybe (Domain.Types.PurchasedPass.StatusType) -> Environment.FlowHandler [API.Types.UI.Pass.PurchasedPassAPIEntity])
getPassCustomerPurchasedPasses a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerPurchasedPasses a5 a4 a3 a2 a1

getPassCustomerTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler [API.Types.UI.Pass.PurchasedPassTransactionAPIEntity])
getPassCustomerTransactions a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerTransactions a5 a4 a3 a2 a1

postPassCustomerActivateToday :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Data.Time.Day) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassCustomerActivateToday a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerActivateToday a5 a4 a3 a2 a1

postPassCustomerPassSelect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> API.Types.Dashboard.AppManagement.Pass.PurchasedPassSelectReq -> Environment.FlowHandler API.Types.UI.Pass.PassSelectionAPIEntity)
postPassCustomerPassSelect a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerPassSelect a5 a4 a3 a2 a1

getPassCustomerPaymentStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> Environment.FlowHandler Lib.Payment.Domain.Action.PaymentStatusResp)
getPassCustomerPaymentStatus a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.getPassCustomerPaymentStatus a4 a3 a2 a1

postPassCustomerPassResetDeviceSwitchCount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassCustomerPassResetDeviceSwitchCount a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerPassResetDeviceSwitchCount a4 a3 a2 a1

postPassCustomerPassUpdateProfilePicture :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> API.Types.Dashboard.AppManagement.Pass.UpdateProfilePictureReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassCustomerPassUpdateProfilePicture a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Pass.postPassCustomerPassUpdateProfilePicture a5 a4 a3 a2 a1
