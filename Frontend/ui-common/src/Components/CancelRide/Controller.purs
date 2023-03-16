{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.CancelRide.Controller where

import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Maybe
import Common.Types.App (CancellationReasons)


data Action = Button1 PrimaryButtonController.Action
            | Button2 PrimaryButtonController.Action
            | UpdateIndex Int
            | NoAction
            | OnGoBack
            | TextChanged String String
            | ClearOptions
            
type Config = {
    cancelRideReasons :: Array CancellationReasons
    , primaryButtonTextConfig :: PrimaryButtonTextConfig
    , activeIndex :: Maybe Int
    , activeReasonCode :: Maybe String
    , isCancelButtonActive :: Boolean
    , isMandatoryTextHidden :: Boolean
    , isLimitExceeded :: Boolean
    , headingText  :: String 
    , subHeadingText :: String
    , showAllOptionsText :: String
    , hint :: String
    , strings :: StringConfig
}

type StringConfig = {
      mandatory :: String 
    , limitReached :: String 
}

type PrimaryButtonTextConfig = 
  {
      firstText :: String
    , secondText :: String
  }

config :: Config
config = 
  {
    cancelRideReasons : []
    , showAllOptionsText : "Show All Options"
    , primaryButtonTextConfig :
        {
            firstText : "Go Back"
          , secondText : "Cancel Ride"
        }
    , activeIndex : Nothing  
    , activeReasonCode : Nothing
    , isCancelButtonActive : false
    , isMandatoryTextHidden : true
    , isLimitExceeded : false
    , headingText : "Cancel Ride"
    , subHeadingText : "Please tell us why you want to cancel"
    , hint : "Help us with your reason"
    , strings: {
        mandatory : "Mandatory"
      , limitReached : "Max character limit reached 100 of 100"
    }
}