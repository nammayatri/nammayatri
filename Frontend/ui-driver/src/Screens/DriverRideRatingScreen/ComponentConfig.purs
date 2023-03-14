module Screens.DriverRideRatingScreen.ComponentConfig where

import Components.PrimaryButton as PrimaryButton
import Language.Strings
import Language.Types (STR(..))
import PrestoDOM
import Screens.Types as ST
import Styles.Colors as Color

primaryButtonConfig :: ST.DriverRideRatingScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString SUBMIT)}
      , width = MATCH_PARENT
      , margin = (Margin 0 0 0 0)
      , cornerRadius = 0.0
      , background = Color.black900
      , height = (V 64)
      }
  in primaryButtonConfig'