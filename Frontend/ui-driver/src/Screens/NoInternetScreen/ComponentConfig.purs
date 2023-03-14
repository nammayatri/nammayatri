module Screens.NoInternetScreen.ComponentConfig where

import Common.Types.App
import Components.PrimaryButton as PrimaryButton
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings
import Language.Types (STR(..))
import Prelude
import PrestoDOM
import Styles.Colors as Color

primaryButtonConfig :: String -> PrimaryButton.Config 
primaryButtonConfig triggertype = let
    config' = PrimaryButton.config 
    primaryButtonConfig' = config' 
      { textConfig 
        { text = if triggertype == "INTERNET_ACTION" then (getString TRY_AGAIN) else (getString GRANT_ACCESS)
        , fontStyle = FontStyle.bold LanguageStyle
        , textSize = FontSize.a_16
        , color = Color.yellow900
        }
      , width = MATCH_PARENT 
      , background = Color.black900
      , margin = (Margin 0 0 0 0)
      , cornerRadius = 0.0
      }
  in primaryButtonConfig'