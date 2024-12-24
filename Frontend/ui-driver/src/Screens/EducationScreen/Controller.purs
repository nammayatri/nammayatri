module Screens.EducationScreen.Controller where

import Prelude
import PrestoDOM
import Language.Strings(getString)
import Data.Array as Array
import Language.Types (STR(..))
import Data.Maybe
import Log
import Styles.Colors as Color
import JBridge (deletePopUpCallBack)
import Effect.Class (liftEffect)
import Data.String (take)
import Helpers.Utils as HU
import Screens (ScreenName(..), getScreen)
import Timers (clearTimerWithId)
import Components.GenericHeader.Controller as GenericHeader
import Components.PrimaryButton.Controller as PrimaryButton
import Components.GenericHeader as GenericHeader
import Screens.Types (EducationScreenState(..))

data Action = 
    GoBack
  | GenericHeaderAC GenericHeader.Action
  | AfterRender
  | NoAction
  | PrimaryButtonActionController PrimaryButton.Action
  | YoutubeVideoStatus String

data ScreenOutput =
    GoToHomeScreen

eval :: Action -> EducationScreenState -> Eval Action ScreenOutput EducationScreenState

eval _ state = update state

primaryButtonConfig :: EducationScreenState -> PrimaryButton.Config
primaryButtonConfig state =
    let
        config = PrimaryButton.config
        primaryButtonConfig' = config
            { textConfig
                { text = ""
                , color = Color.yellow900
                , height = V 40
                }
            , margin = (Margin 16 10 16 10)
            , isClickable = true
            , id = "WhereIsMyBusPrimaryButton"
            , enableRipple = true
            , rippleColor = Color.rippleShade
            }
    in
        primaryButtonConfig'


genericHeaderConfig :: EducationScreenState -> GenericHeader.Config
genericHeaderConfig state = let
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , width = WRAP_CONTENT
    , background = Color.white900
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_chevron_left"
      , margin = (Margin 12 12 12 12)
      }
    , textConfig {
        text = ""
      , color = Color.black800
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , padding = (Padding 0 5 0 0)
    }
  in genericHeaderConfig'