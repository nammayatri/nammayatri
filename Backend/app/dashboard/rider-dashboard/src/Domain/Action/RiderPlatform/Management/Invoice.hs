{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.RiderPlatform.Management.Invoice
  ( getInvoiceInvoice,
    getInvoiceFinanceList,
    getInvoiceFinancePdf,
  )
where

import qualified API.Client.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Invoice
import qualified Data.Text
import "rider-app" Domain.Types.AccessMatrix
import qualified "beckn-spec" Domain.Types.Invoice
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.Invoice
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant

getInvoiceInvoice :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.UTCTime -> Data.Text.Text -> Kernel.Prelude.UTCTime -> Environment.Flow [API.Types.RiderPlatform.Management.Invoice.InvoiceRes])
getInvoiceInvoice merchantShortId opCity apiTokenInfo from phoneNumber to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.invoiceDSL.getInvoiceInvoice) from phoneNumber to

getInvoiceFinanceList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Domain.Types.Invoice.InvoiceType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Lib.Finance.Domain.Types.Invoice.InvoiceStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.Flow API.Types.RiderPlatform.Management.Invoice.FinanceInvoiceListRes)
getInvoiceFinanceList merchantShortId opCity apiTokenInfo from invoiceId invoiceNumber invoiceType limit offset status to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.invoiceDSL.getInvoiceFinanceList) from invoiceId invoiceNumber invoiceType limit offset status to

getInvoiceFinancePdf :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Data.Text.Text -> Environment.Flow API.Types.RiderPlatform.Management.Invoice.FinanceInvoicePdfRes)
getInvoiceFinancePdf merchantShortId opCity apiTokenInfo invoiceId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.invoiceDSL.getInvoiceFinancePdf) invoiceId
