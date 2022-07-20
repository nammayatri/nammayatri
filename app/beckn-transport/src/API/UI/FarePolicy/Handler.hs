module API.UI.FarePolicy.Handler (API, handler) where

import qualified API.UI.FarePolicy.Discount.Handler as Discount
import qualified API.UI.FarePolicy.FareProduct.Handler as FareProduct
import qualified API.UI.FarePolicy.OneWayFarePolicy.Handler as OneWay
import qualified API.UI.FarePolicy.RentalFarePolicy.Handler as Rentals
import App.Types
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
