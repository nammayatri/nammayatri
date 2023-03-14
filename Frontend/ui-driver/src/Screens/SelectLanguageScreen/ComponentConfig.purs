module Screens.SelectLanguageScreen.ComponentConfig where

import Components.PrimaryButton as PrimaryButton
import Font.Size as FontSize
import Language.Strings
import Language.Types (STR(..))
import PrestoDOM
import Styles.Colors as Color
import Screens.Types as ST

primaryButtonConfig :: ST.SelectLanguageScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig
      { text = (getString UPDATE)
      , color = Color.primaryButtonColor
      , textSize = FontSize.a_18}
      , margin = (Margin 0 0 0 0)
      , cornerRadius = 0.0
      , background = Color.black900
      , height = (V 60)
      }
  in primaryButtonConfig'