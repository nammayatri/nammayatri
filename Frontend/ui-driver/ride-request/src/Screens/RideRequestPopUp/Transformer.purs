module Screens.RideRequestPopUp.TransFormer where

import Prelude

import Api.Types (SearchRequest(..))
import Data.Maybe (fromMaybe)
import Helpers.Accessor
import PrestoDOM (toPropValue)
import Screens.RideRequestPopUp.ScreenData (PopupProps)
import Data.Lens ((^.))

toPopupProp :: Array SearchRequest -> Array PopupProps
toPopupProp =
  map
    ( \(SearchRequest item) ->
        { tripPrice: toPropValue $ show item.baseFare
        , tripDistance: toPropValue $ show $ fromMaybe 0 item.distance
        , pickupDistance: toPropValue $ show $ fromMaybe 0 item.distanceToPickup
        , sourceArea: toPropValue $ fromMaybe "" $ item.fromLocation ^. _area
        , sourceFullAddress: toPropValue $ fromMaybe "" $ item.fromLocation ^. _full_address
        , sourcePincode: toPropValue $ fromMaybe "" $ item.fromLocation ^. _areaCode
        , destinationArea: toPropValue $ fromMaybe "" $ item.toLocation ^. _area
        , destinationFullAddress: toPropValue $ fromMaybe "" $ item.toLocation ^. _full_address
        , destinationPincode: toPropValue $ fromMaybe "" $ item.toLocation ^. _areaCode
        }
    )
