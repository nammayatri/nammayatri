module API.RiderPlatform
  ( API,
    handler,
  )
where

import qualified API.RiderPlatform.Customer as Customer
import qualified API.RiderPlatform.Merchant as Merchant
import qualified "lib-dashboard" Domain.Types.Merchant as DMerchant
import "lib-dashboard" Environment
import Kernel.Types.Id
import Servant

type API =
  "bap"
    :> Capture "merchantId" (ShortId DMerchant.Merchant)
    :> ( Customer.API
           :<|> Merchant.API
       )

handler :: FlowServer API
handler merchantId =
  Customer.handler merchantId
    :<|> Merchant.handler merchantId
