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
import Prelude (class Eq, class Show, show, (<>))
import Common.Types.App (RateCardType(..), FareList(..), WaitingTimeInfo(..))
import Components.PrimaryButton as PrimaryButton
import Data.Maybe(Maybe(..))
import PrestoDOM (Length(..), Margin(..), Visibility(..))
import Styles.Colors as Color

instance showAction :: Show Action where
  show (Close) = "Close"
  show (BackPressed) = "BackPressed"
  show (NoAction) = "NoAction"
  show (GoToDefaultStart) = "GoToDefaultStart"
  show (GoToDriverAddition) = "GoToDriverAddition"
  show (GoToFareUpdate) = "GoToFareUpdate"
  show (GoToWaitingCharges) = "GoToWaitingCharges"
  show (GoToTollOrParkingCharges) = "GoToTollOrParkingCharges"
  show (GoToDriverAllowance) = "GoToDriverAllowance"
  show (GoToNightShiftCharges) = "GoToNightShiftCharges"
  show (GoToTollAndParkingCharges) = "GoToTollAndParkingCharges"
  show (PrimaryButtonAC var1) = "PrimaryButtonAC_" <> show var1

data Action = Close 
              | BackPressed 
              | NoAction 
              | GoToDefaultStart 
              | GoToDriverAddition 
              | GoToFareUpdate 
              | GoToWaitingCharges
              | GoToTollOrParkingCharges
              | GoToDriverAllowance
              | GoToNightShiftCharges
              | GoToTollAndParkingCharges
              | PrimaryButtonAC PrimaryButton.Action


type Config = {
    additionalFare :: String,
    isNightShift :: Boolean,
    currentRateCardType :: RateCardType,
    fareInfoDescription :: Array String,
    onFirstPage :: Boolean,
    showDetails :: Boolean,
    title :: String,
    description :: String,
    buttonText :: Maybe String,
    applicableCharges :: String,
    fareList :: Array FareList,
    otherOptions :: Array FareList,
    driverAdditions :: Array FareList,
    additionalStrings :: Array FareList,
    fareInfoText :: String,
    waitingTimeInfo :: WaitingTimeInfo,
    driverAdditionsImage :: String,
    primaryButtonConfig :: ButtonConfig
}

type ButtonConfig =
  { margin :: Margin
  , text :: String
  , color :: String
  , height :: Length
  , cornerRadius :: Number
  , background :: String
  , visibility :: Visibility
  , enableRipple :: Boolean
  , rippleColor :: String
  }

config :: Config 
config = {
    isNightShift : false,
    additionalFare : "₹30",
    currentRateCardType : DefaultRateCard,
    onFirstPage : false,
    showDetails : true,
    title : "",
    description : "",
    fareInfoDescription: [],
    buttonText : Nothing,
    driverAdditions : [],
    applicableCharges : "",
    driverAdditionsImage : "",
    fareList : [],
    otherOptions : [],
    additionalStrings : [],
    fareInfoText : "",
    waitingTimeInfo : { freeMinutes: "", charge: "" },
    primaryButtonConfig : {
      text: ""
    , color : Color.yellow900
    , margin : MarginVertical 20 10
    , cornerRadius : 8.0
    , background : Color.black900
    , height : V 54
    , visibility : GONE
    , enableRipple : true
    , rippleColor : Color.rippleShade
    }
}
