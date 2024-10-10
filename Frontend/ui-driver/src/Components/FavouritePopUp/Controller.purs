module Components.FavouritePopUp.Controller where

import Components.PrimaryButton.Controller as PrimaryButton
import Language.Strings (getString)
import Language.Types (STR(..))
import Styles.Colors as Color
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), LetterSpacing(..), Accessiblity(..), accessibility)
data Action = OnClickDone PrimaryButton.Action

type Config = {
  title :: String,
  message :: String
}

config :: Config
config = {
  title : "",
  message : ""
}

doneButtonConfig :: Config -> PrimaryButton.Config
doneButtonConfig config = let
    primaryButtonConfig' =  PrimaryButton.config
      { textConfig
        { text = getString GOT_IT
        , color = Color.primaryButtonColor
        }
      , cornerRadius = 8.0
      , background = Color.black900
      , height = V 48
      , alpha = 1.0 
      , isClickable = true 
      , margin = MarginTop 15
      }
  in primaryButtonConfig'