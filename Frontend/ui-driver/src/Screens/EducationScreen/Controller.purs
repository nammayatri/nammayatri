module Screens.EducationScreen.Controller where

import Prelude
import PrestoDOM
import Language.Strings(getString)
import Data.Array as Array
import Language.Types (STR(..))
import Data.Maybe
import Log
import Styles.Colors as Color
import Data.Function.Uncurried (runFn5)
import Effect.Uncurried (runEffectFn1)
import Engineering.Helpers.Commons (getNewIDWithTag, getVideoID, getYoutubeData)
import JBridge (deletePopUpCallBack, releaseYoutubeView, setYoutubePlayer, removeMediaPlayer, pauseYoutubeVideo)
import Effect.Class (liftEffect)
import Data.String (take)
import Helpers.Utils as HU
import Screens (ScreenName(..), getScreen)
import Timers (clearTimerWithId)
import Components.GenericHeader.Controller as GenericHeader
import Components.PrimaryButton.Controller as PrimaryButton
import Components.GenericHeader as GenericHeader
import Screens.Types (EducationScreenState(..))
import PrestoDOM (class Loggable, Eval, update, continue, exit, continueWithCmd, updateAndExit, updateWithCmdAndExit)

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
        _ -> pure unit

data Action = 
    GoBack
  | GenericHeaderAC GenericHeader.Action
  | AfterRender
  | NoAction
  | PrimaryButtonActionController PrimaryButton.Action
  | YoutubeVideoStatus String

data ScreenOutput =
    OnButtonClick
  | GoToHomeScreen

eval :: Action -> EducationScreenState -> Eval Action ScreenOutput EducationScreenState

eval GoBack state = do
    exit GoToHomeScreen
  
eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = exit GoToHomeScreen
eval (PrimaryButtonActionController PrimaryButton.OnClick) state = do
  exit OnButtonClick

eval _ state = update state

primaryButtonConfig :: EducationScreenState -> PrimaryButton.Config
primaryButtonConfig state =
    let
        config = PrimaryButton.config
        primaryButtonConfig' = config
            { textConfig
                { text = state.buttonText
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
        text = state.headerText
      , color = Color.black800
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , padding = (Padding 0 5 0 0)
    }
  in genericHeaderConfig'