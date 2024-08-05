{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking.Invoice
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.RideBooking.Invoice
import qualified API.Types.UI.Invoice
import qualified Data.Text
import qualified Domain.Action.RiderPlatform.RideBooking.Invoice
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("invoice" :> GetInvoiceInvoice)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getInvoiceInvoice merchantId city

type GetInvoiceInvoice = (ApiAuth ('APP_BACKEND) ('CUSTOMERS) ('INVOICE) :> API.Types.RiderPlatform.RideBooking.Invoice.GetInvoiceInvoice)

getInvoiceInvoice :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.UTCTime -> Data.Text.Text -> Kernel.Prelude.UTCTime -> Environment.FlowHandler [API.Types.UI.Invoice.InvoiceRes])
getInvoiceInvoice merchantShortId opCity apiTokenInfo from phoneNumber to = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Invoice.getInvoiceInvoice merchantShortId opCity apiTokenInfo from phoneNumber to
