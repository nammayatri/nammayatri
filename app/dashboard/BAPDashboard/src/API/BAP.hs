module API.BAP
  ( API,
    handler,
  )
where

import qualified API.BAP.Customer as Customer
import Beckn.Types.Id
import qualified "lib-dashboard" Domain.Types.Merchant as DMerchant
import "lib-dashboard" Environment
import Servant

type API =
  "bap"
    :> Capture "merchantId" (ShortId DMerchant.Merchant)
    :> Customer.API

handler :: FlowServer API
handler =
  Customer.handler
