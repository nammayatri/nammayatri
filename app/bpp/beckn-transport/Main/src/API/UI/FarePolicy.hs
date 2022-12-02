module API.UI.FarePolicy (API, handler) where

import qualified API.UI.FarePolicy.Discount as Discount
import qualified API.UI.FarePolicy.FareProduct as FareProduct
import qualified API.UI.FarePolicy.OneWayFarePolicy as OneWay
import qualified API.UI.FarePolicy.RentalFarePolicy as Rentals
import Environment
import Servant

type API =
  "org"
    :> ( FareProduct.API
           :<|> "farePolicy"
           :> ( Discount.API
                  :<|> Rentals.API
                  :<|> OneWay.API
              )
       )

handler :: FlowServer API
handler =
  FareProduct.handler
    :<|> ( Discount.handler
             :<|> Rentals.handler
             :<|> OneWay.handler
         )
