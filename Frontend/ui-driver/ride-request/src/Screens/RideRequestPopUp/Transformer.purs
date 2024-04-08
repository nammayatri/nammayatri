module Screens.RideRequestPopUp.TransFormer where

import Prelude
import Api.Types (SearchRequest(..))
import Data.Maybe (fromMaybe)
import PrestoDOM (toPropValue)
import Screens.RideRequestPopUp.ScreenData (PopupProps)

toPopupProp :: Array SearchRequest -> Array PopupProps
toPopupProp =
  map
    ( \(SearchRequest item) ->
        { tripPrice: toPropValue item.baseFare
        , tripDistance: toPropValue $ fromMaybe 0 item.distance
        , pickupDistance: toPropValue $ fromMaybe 0 item.distanceToPickup
        , sourceArea: toPropValue "source Area"
        , sourceFullAddress: toPropValue "sourceFullAddress"
        , sourcePincode: toPropValue "sourcePincode"
        , destinationArea: toPropValue "destinationArea"
        , destinationFullAddress: toPropValue "destinationFullAddress"
        , destinationPincode: toPropValue "destinationPincode"
        }
    )
