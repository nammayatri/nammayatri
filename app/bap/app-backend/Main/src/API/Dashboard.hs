module API.Dashboard where

import qualified API.Dashboard.Customer as Customer
import Beckn.Types.Id
import qualified Domain.Types.Merchant as DM
import Environment
import Servant hiding (throwError)
import Tools.Auth (DashboardTokenAuth)

type API =
  "dashboard"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> API'

-- TODO do we need different tokens for different merchants? now we have one common token
type API' =
  DashboardTokenAuth
    :> Customer.API

handler :: FlowServer API
handler merchantId _dashboard =
  Customer.handler merchantId
