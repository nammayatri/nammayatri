{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.Pass
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.Pass
import qualified "rider-app" API.Types.UI.Pass
import qualified Data.Time
import qualified Domain.Action.RiderPlatform.AppManagement.Pass
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Pass
import qualified "rider-app" Domain.Types.Person
import qualified "rider-app" Domain.Types.PurchasedPass
import qualified "rider-app" Domain.Types.PurchasedPassPayment
import qualified "lib-dashboard" Environment
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
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("pass" :> (GetPassCustomerAvailablePasses :<|> GetPassCustomerPurchasedPasses :<|> GetPassCustomerTransactions :<|> PostPassCustomerActivateToday :<|> PostPassCustomerPassSelect :<|> GetPassCustomerPaymentStatus :<|> PostPassCustomerPassResetDeviceSwitchCount :<|> PostPassCustomerPassUpdateProfilePicture))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPassCustomerAvailablePasses merchantId city :<|> getPassCustomerPurchasedPasses merchantId city :<|> getPassCustomerTransactions merchantId city :<|> postPassCustomerActivateToday merchantId city :<|> postPassCustomerPassSelect merchantId city :<|> getPassCustomerPaymentStatus merchantId city :<|> postPassCustomerPassResetDeviceSwitchCount merchantId city :<|> postPassCustomerPassUpdateProfilePicture merchantId city

type GetPassCustomerAvailablePasses =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASS / 'API.Types.Dashboard.AppManagement.Pass.GET_PASS_CUSTOMER_AVAILABLE_PASSES)
      :> API.Types.Dashboard.AppManagement.Pass.GetPassCustomerAvailablePasses
  )

type GetPassCustomerPurchasedPasses =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASS / 'API.Types.Dashboard.AppManagement.Pass.GET_PASS_CUSTOMER_PURCHASED_PASSES)
      :> API.Types.Dashboard.AppManagement.Pass.GetPassCustomerPurchasedPasses
  )

type GetPassCustomerTransactions =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASS / 'API.Types.Dashboard.AppManagement.Pass.GET_PASS_CUSTOMER_TRANSACTIONS)
      :> API.Types.Dashboard.AppManagement.Pass.GetPassCustomerTransactions
  )

type PostPassCustomerActivateToday =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASS / 'API.Types.Dashboard.AppManagement.Pass.POST_PASS_CUSTOMER_ACTIVATE_TODAY)
      :> API.Types.Dashboard.AppManagement.Pass.PostPassCustomerActivateToday
  )

type PostPassCustomerPassSelect =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASS / 'API.Types.Dashboard.AppManagement.Pass.POST_PASS_CUSTOMER_PASS_SELECT)
      :> API.Types.Dashboard.AppManagement.Pass.PostPassCustomerPassSelect
  )

type GetPassCustomerPaymentStatus =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASS / 'API.Types.Dashboard.AppManagement.Pass.GET_PASS_CUSTOMER_PAYMENT_STATUS)
      :> API.Types.Dashboard.AppManagement.Pass.GetPassCustomerPaymentStatus
  )

type PostPassCustomerPassResetDeviceSwitchCount =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASS / 'API.Types.Dashboard.AppManagement.Pass.POST_PASS_CUSTOMER_PASS_RESET_DEVICE_SWITCH_COUNT)
      :> API.Types.Dashboard.AppManagement.Pass.PostPassCustomerPassResetDeviceSwitchCount
  )

type PostPassCustomerPassUpdateProfilePicture =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.PASS) / ('API.Types.Dashboard.AppManagement.Pass.POST_PASS_CUSTOMER_PASS_UPDATE_PROFILE_PICTURE))
      :> API.Types.Dashboard.AppManagement.Pass.PostPassCustomerPassUpdateProfilePicture
  )

getPassCustomerAvailablePasses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Environment.FlowHandler [API.Types.UI.Pass.PassInfoAPIEntity])
getPassCustomerAvailablePasses merchantShortId opCity apiTokenInfo customerId language = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Pass.getPassCustomerAvailablePasses merchantShortId opCity apiTokenInfo customerId language

getPassCustomerPurchasedPasses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Kernel.Prelude.Maybe Domain.Types.PurchasedPass.StatusType -> Environment.FlowHandler [API.Types.UI.Pass.PurchasedPassAPIEntity])
getPassCustomerPurchasedPasses merchantShortId opCity apiTokenInfo customerId language status = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Pass.getPassCustomerPurchasedPasses merchantShortId opCity apiTokenInfo customerId language status

getPassCustomerTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler [API.Types.UI.Pass.PurchasedPassTransactionAPIEntity])
getPassCustomerTransactions merchantShortId opCity apiTokenInfo customerId limit offset = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Pass.getPassCustomerTransactions merchantShortId opCity apiTokenInfo customerId limit offset

postPassCustomerActivateToday :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment) -> Kernel.Prelude.Maybe Data.Time.Day -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassCustomerActivateToday merchantShortId opCity apiTokenInfo customerId passNumber purchasedPassPaymentId startDay = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Pass.postPassCustomerActivateToday merchantShortId opCity apiTokenInfo customerId passNumber purchasedPassPaymentId startDay

postPassCustomerPassSelect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> API.Types.Dashboard.AppManagement.Pass.PurchasedPassSelectReq -> Environment.FlowHandler API.Types.UI.Pass.PassSelectionAPIEntity)
postPassCustomerPassSelect merchantShortId opCity apiTokenInfo customerId passId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Pass.postPassCustomerPassSelect merchantShortId opCity apiTokenInfo customerId passId req

getPassCustomerPaymentStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> Environment.FlowHandler Lib.Payment.Domain.Action.PaymentStatusResp)
getPassCustomerPaymentStatus merchantShortId opCity apiTokenInfo customerId orderId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Pass.getPassCustomerPaymentStatus merchantShortId opCity apiTokenInfo customerId orderId

postPassCustomerPassResetDeviceSwitchCount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassCustomerPassResetDeviceSwitchCount merchantShortId opCity apiTokenInfo customerId passId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Pass.postPassCustomerPassResetDeviceSwitchCount merchantShortId opCity apiTokenInfo customerId passId

postPassCustomerPassUpdateProfilePicture :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> API.Types.Dashboard.AppManagement.Pass.UpdateProfilePictureReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassCustomerPassUpdateProfilePicture merchantShortId opCity apiTokenInfo customerId purchasedPassId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Pass.postPassCustomerPassUpdateProfilePicture merchantShortId opCity apiTokenInfo customerId purchasedPassId req
