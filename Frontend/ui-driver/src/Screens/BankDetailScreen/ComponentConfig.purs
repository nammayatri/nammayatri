module Screens.BankDetailScreen.ComponentConfig where

import Components.PrimaryButton as PrimaryButton
import Language.Strings
import Language.Types (STR(..))
import PrestoDOM
import Screens.Types as ST
import Styles.Colors as Color

primaryButtonConfig :: ST.BankDetailScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString NEXT), textSize = 16}
      , width = MATCH_PARENT
      , cornerRadius = 0.0
      , height = (V 64)
      , background = Color.black900
      , margin = (Margin 0 0 0 0)
      }
  in primaryButtonConfig'