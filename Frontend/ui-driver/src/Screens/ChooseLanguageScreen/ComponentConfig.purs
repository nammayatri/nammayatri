module Screens.ChooseLanguageScreen.ComponentConfig where

import Components.PrimaryButton as PrimaryButton
import PrestoDOM
import Screens.Types as ST

primaryButtonViewConfig :: ST.ChooseLanguageScreenState -> PrimaryButton.Config
primaryButtonViewConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = "Next" }
      , id = "PrimaryButtonLanguage"
      , isClickable = true
      , height = (V 60)
      , cornerRadius = 0.0
      , margin = (Margin 0 0 0 0)
      }
  in primaryButtonConfig'