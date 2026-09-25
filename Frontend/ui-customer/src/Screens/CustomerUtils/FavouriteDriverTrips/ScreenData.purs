module Screens.CustomerUtils.FavouriteDriverTrips.ScreenData where

import Prelude

import Screens.Types (FavouriteDriverTripsState, PaymentMode(..), FavouriteDriverTripsType(..), Details)
import ConfigProvider
import Data.Maybe(Maybe(..))

initData :: FavouriteDriverTripsState
initData = {
  data: {
    driverNumber : ""
  , driverName : ""
  , driverId : Nothing
  , details : []
  }
}