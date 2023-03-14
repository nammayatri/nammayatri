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