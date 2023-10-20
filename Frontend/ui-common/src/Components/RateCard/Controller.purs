{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RateCard.Controller where

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Prelude (class Eq, class Show)
import Common.Types.App (RateCardType(..), FareList(..))
import Components.PrimaryButton as PrimaryButton
import Data.Maybe(Maybe(..))

data Action = Close 
              | BackPressed 
              | NoAction 
              | GoToDefaultStart 
              | GoToDriverAddition 
              | GoToFareUpdate 
              | PrimaryButtonAC PrimaryButton.Action


type Config = {
    baseFare :: String,
    extraFare :: String,
    pickUpCharges :: String,
    additionalFare :: String,
    nightCharges :: Boolean,
    nightShiftMultiplier :: String, 
    currentRateCardType :: RateCardType,
    onFirstPage :: Boolean,
    showDetails :: Boolean,
    alertDialogPrimaryColor :: String,
    title :: String,
    description :: String,
    buttonText :: Maybe String,
    applicableCharges :: String,
    primaryButtonText :: String,
    fareList :: Array FareList,
    otherOptions :: Array FareList,
    additionalStrings :: Array FareList,
    driverAdditionsImage :: String,
    fareInfoText :: String,
    rentalPackage :: Boolean,
    rentalDetails :: RentalConfig
}

type RentalConfig = {
    title :: String
  , titleHeight :: String
  , titleSuffixImage :: String
  , farePackageImage :: String
  , buttonText :: String
}

config :: Config 
config = {
    baseFare : "₹45",
    extraFare : "₹23",
    pickUpCharges : "₹10", 
    additionalFare : "₹30",
    nightCharges : false,
    nightShiftMultiplier : "1.5",
    currentRateCardType : DefaultRateCard,
    onFirstPage : false,
    showDetails : true,
    alertDialogPrimaryColor: "#2194FF",
    title : "",
    description : "",
    buttonText : Nothing,
    primaryButtonText : "",
    applicableCharges : "",
    driverAdditionsImage : "",
    fareList : [],
    otherOptions : [],
    additionalStrings : [],
    fareInfoText : "",
    rentalPackage : false,
    rentalDetails : {
      title: "",
      titleHeight : "",
      titleSuffixImage : "",
      farePackageImage : "",
      buttonText : "Got It!"
    }
}
