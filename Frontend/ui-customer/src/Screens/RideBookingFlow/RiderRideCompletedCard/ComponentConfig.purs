module Screens.RideBookingFlow.RiderRideCompletedCard.Config where

import Components.PrimaryButton as PrimaryButton
import Screens.Types (RiderRideCompletedScreenState)
import Language.Strings (getString)
import JBridge as JB
import Language.Types as LT
import Styles.Colors as Color
import PrestoDOM 
import Prelude
import Data.Maybe
import Components.FavouriteDriverInfoCard.Controller as FavouriteDriverInfoCard

primaryButtonConfig :: RiderRideCompletedScreenState -> PrimaryButton.Config
primaryButtonConfig state = PrimaryButton.config
    { textConfig
        { text = if state.ratingCard.recordAudioState.recordedFile == Nothing then getString LT.SUBMIT else "Save & Submit"
        , color = Color.yellow900
        , accessibilityHint = "Submit Feedback Button" 
        }
    , alpha = if state.ratingCard.recordAudioState.isRecording == true then 0.5 else 1.0
    , background = Color.black900
    , margin = (Margin 0 0 0 0)
    , id = "RateYourDriverButton"
    , enableLoader = (JB.getBtnLoader "RateYourDriverButton")
    , enableRipple = true
    , rippleColor = Color.rippleShade
    , isClickable = not state.ratingCard.recordAudioState.isRecording
    }

driverInfoCardConfig :: RiderRideCompletedScreenState -> FavouriteDriverInfoCard.FavouriteDriverInfoCardState
driverInfoCardConfig state = FavouriteDriverInfoCard.config 