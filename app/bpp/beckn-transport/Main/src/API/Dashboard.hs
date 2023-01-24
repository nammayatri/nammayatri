module API.Dashboard where

import qualified API.Dashboard.Driver as Driver
import qualified API.Dashboard.Merchant as Merchant
import qualified API.Dashboard.Ride as Ride
import Beckn.Types.Id
import qualified Domain.Types.Merchant as DM
import Environment
import Servant
import Tools.Auth

type API =
  "dashboard"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> API'

type API' =
  DashboardTokenAuth
    :> ( Driver.API
           :<|> Ride.API
           :<|> Merchant.API
       )

handler :: FlowServer API
handler merchantId _dashboard =
  Driver.handler merchantId
    :<|> Ride.handler merchantId
    :<|> Merchant.handler merchantId
