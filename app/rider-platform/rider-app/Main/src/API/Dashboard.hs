module API.Dashboard where

import qualified API.Dashboard.Booking as Booking
import qualified API.Dashboard.Customer as Customer
import qualified API.Dashboard.Merchant as Merchant
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Types.Id
import Servant hiding (throwError)
import Tools.Auth (DashboardTokenAuth)

type API =
  "dashboard"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> API'

type API' =
  DashboardTokenAuth
    :> ( Customer.API
           :<|> Booking.API
           :<|> Merchant.API
       )

handler :: FlowServer API
handler merchantId _dashboard =
  Customer.handler merchantId
    :<|> Booking.handler merchantId
    :<|> Merchant.handler merchantId
