{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.RiderPlatform.Management.Invoice (getInvoiceInvoice) where

import qualified API.Client.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Invoice
import qualified Data.Text
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getInvoiceInvoice :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.UTCTime -> Data.Text.Text -> Kernel.Prelude.UTCTime -> Environment.Flow [API.Types.RiderPlatform.Management.Invoice.InvoiceRes])
getInvoiceInvoice merchantShortId opCity apiTokenInfo from phoneNumber to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.invoiceDSL.getInvoiceInvoice) from phoneNumber to
