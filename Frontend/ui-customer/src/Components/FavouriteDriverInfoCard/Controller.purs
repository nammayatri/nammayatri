module Components.FavouriteDriverInfoCard.Controller where

import Prelude
import Components.PrimaryButton.Controller as PrimaryButton
import Styles.Colors as Color
import PrestoDOM.Types.DomAttributes (Length(..), Margin(..))
import Data.Maybe (Maybe(..))
import Language.Strings (getString)
import Language.Types (STR(..))

data Action = Back | OnClickDone PrimaryButton.Action

type FavouriteDriverInfoCardState = {}

config :: FavouriteDriverInfoCardState 
config = {}

donePrimaryButtonConfig :: PrimaryButton.Config
donePrimaryButtonConfig = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig { 
          text = getString GOTIT
        , color = Color.yellow900
        }
      , cornerRadius = 8.0
      , background = Color.black900
      , height = V 48
      , alpha = 1.0 
      , isClickable = true 
      , margin = Margin 16 16 16 16
      , id = "add_audio_model_done_button"
      }
  in primaryButtonConfig'