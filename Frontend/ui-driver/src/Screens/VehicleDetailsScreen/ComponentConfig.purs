module Screens.VehicleDetailsScreen.ComponentConfig where

import Common.Types.App
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..), fromMaybe)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings
import Language.Types (STR(..))
import PrestoDOM
import Styles.Colors as Color
import Screens.Types as ST

------------------------------ primaryButtonConfig ---------------------------------
primaryButtonConfig :: ST.VehicleDetailsScreenState -> PrimaryButton.Config
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
      , isClickable = state.props.deleteButtonVisibility
      , alpha = if state.props.deleteButtonVisibility then 1.0 else 0.7
      }
  in primaryButtonConfig'

------------------------------ primaryEditTextConfig ---------------------------------
primaryEditTextConfig :: String -> String -> PrimaryEditText.Config
primaryEditTextConfig label value = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { singleLine = true
          , pattern = Just "[0-9]*,10"
          , fontStyle = FontStyle.bold LanguageStyle
          , textSize = FontSize.a_16
          , text = value
          , alpha = 0.9
        }
      , topLabel
        { textSize = FontSize.a_14
        , text = label
        , color = Color.greyTextColor
        }
      }
    in primaryEditTextConfig'