module API.Action.RiderPlatform.Management (API, handler) where

import qualified API.Action.RiderPlatform.Management.Booking
import qualified API.Action.RiderPlatform.Management.Invoice
import qualified API.Action.RiderPlatform.Management.Merchant
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id
import Servant

type API =
  API.Action.RiderPlatform.Management.Booking.API
    :<|> API.Action.RiderPlatform.Management.Invoice.API
    :<|> API.Action.RiderPlatform.Management.Merchant.API

handler :: ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API
handler merchantId city =
  API.Action.RiderPlatform.Management.Booking.handler merchantId city
    :<|> API.Action.RiderPlatform.Management.Invoice.handler merchantId city
    :<|> API.Action.RiderPlatform.Management.Merchant.handler merchantId city
