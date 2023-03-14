module Screens.EnterMobileNumberScreen.ComponentConfig where

import Components.PrimaryButton as PrimaryButton
import Language.Strings
import Language.Types (STR(..))
import PrestoDOM
import Screens.Types as ST

primaryButtonViewConfig :: ST.EnterMobileNumberScreenState -> PrimaryButton.Config
primaryButtonViewConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString NEXT) }
      , id = "PrimaryButtonMobileNumber"
      , isClickable = state.props.btnActive
      , alpha = if state.props.btnActive then 1.0 else 0.6
      , height = (V 60)
      , cornerRadius = 0.0
      , margin = (Margin 0 0 0 0)
      }
  in primaryButtonConfig'