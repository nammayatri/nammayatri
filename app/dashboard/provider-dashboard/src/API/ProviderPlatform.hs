module API.ProviderPlatform
  ( API,
    handler,
  )
where

import qualified API.ProviderPlatform.DynamicOfferDriver as DynamicOfferDriver
import qualified API.ProviderPlatform.StaticOfferDriver as StaticOfferDriver
import "lib-dashboard" Environment
import Servant

type API =
  "bpp"
    :> ( StaticOfferDriver.API
           :<|> DynamicOfferDriver.API
       )

handler :: FlowServer API
handler =
  StaticOfferDriver.handler
    :<|> DynamicOfferDriver.handler
