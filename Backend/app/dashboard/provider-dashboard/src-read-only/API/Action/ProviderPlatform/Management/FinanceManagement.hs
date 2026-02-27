{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.FinanceManagement
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.FinanceManagement
import qualified Domain.Action.ProviderPlatform.Management.FinanceManagement
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("financeManagement" :> (GetFinanceManagementSubscriptionPurchaseList :<|> GetFinanceManagementFleetOperatorList :<|> GetFinanceManagementInvoiceList))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFinanceManagementSubscriptionPurchaseList merchantId city :<|> getFinanceManagementFleetOperatorList merchantId city :<|> getFinanceManagementInvoiceList merchantId city

type GetFinanceManagementSubscriptionPurchaseList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_SUBSCRIPTION_PURCHASE_LIST)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementSubscriptionPurchaseList
  )

type GetFinanceManagementFleetOperatorList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FLEET_OPERATOR_LIST)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFleetOperatorList
  )

type GetFinanceManagementInvoiceList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_INVOICE_LIST)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementInvoiceList
  )

getFinanceManagementSubscriptionPurchaseList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.SubscriptionPurchaseListRes)
getFinanceManagementSubscriptionPurchaseList merchantShortId opCity apiTokenInfo amountMax amountMin driverId fleetOperatorId from limit offset serviceName status subscriptionId to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementSubscriptionPurchaseList merchantShortId opCity apiTokenInfo amountMax amountMin driverId fleetOperatorId from limit offset serviceName status subscriptionId to

getFinanceManagementFleetOperatorList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.FleetOperatorListRes)
getFinanceManagementFleetOperatorList merchantShortId opCity apiTokenInfo fleetOperatorId from limit offset settlementStatus to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFleetOperatorList merchantShortId opCity apiTokenInfo fleetOperatorId from limit offset settlementStatus to

getFinanceManagementInvoiceList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.InvoiceListRes)
getFinanceManagementInvoiceList merchantShortId opCity apiTokenInfo from invoiceId invoiceType limit offset status to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementInvoiceList merchantShortId opCity apiTokenInfo from invoiceId invoiceType limit offset status to
