module API.ProviderPlatform.StaticOfferDriver
  ( API,
    handler,
  )
where

import qualified API.ProviderPlatform.StaticOfferDriver.Driver as Driver
import qualified API.ProviderPlatform.StaticOfferDriver.Merchant as Merchant
import qualified API.ProviderPlatform.StaticOfferDriver.Ride as Ride
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Kernel.Types.Id
import Servant

type API =
  "beckn-transport"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> ( Driver.API
           :<|> Ride.API
           :<|> Merchant.API
       )

handler :: FlowServer API
handler merchantId =
  Driver.handler merchantId
    :<|> Ride.handler merchantId
    :<|> Merchant.handler merchantId
