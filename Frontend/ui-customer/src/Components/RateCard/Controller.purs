{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RateCard.Controller where

data Action = Close | BackPressed | NoAction

type Config = {
    baseFare :: String,
    extraFare :: String,
    pickUpCharges :: String,
    additionalFare :: String,
    nightCharges :: Boolean,
    nightShiftMultiplier :: String 
}

config :: Config 
config = {
    baseFare : "₹45",
    extraFare : "₹23",
    pickUpCharges : "₹10", 
    additionalFare : "₹30",
    nightCharges : false,
    nightShiftMultiplier : "1.5"
}
