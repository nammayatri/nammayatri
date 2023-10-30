module Screens.WelcomeScreen.Controller where

import Components.GenericHeader as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.SelectMenuButton.Controller (Action(..)) as MenuButtonController
import JBridge (minimizeApp, firebaseLogEvent)
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, pure, ($), (==), (||))
import PrestoDOM (Eval, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.Types (WelcomeScreenStage(..), WelcomeScreenState)

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "WelcomeScreen"
    BackPressed -> trackAppBackPress appId (getScreen HOME_SCREEN)
    PrimaryButtonAC act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen WELCOME_SCREEN) "primary_button" "onclick"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen WELCOME_SCREEN) "primary_button" "no_action"
    _ -> trackAppActionClick appId (getScreen WELCOME_SCREEN) "primary_button" "no_action" --change this


data Action = BackPressed
            | AfterRender
            | PrimaryButtonAC PrimaryButtonController.Action
            | MenuButtonAction MenuButtonController.Action
            | GenericHeaderAC GenericHeaderController.Action
            | ChangeStage WelcomeScreenStage

data ScreenOutput = MobileNumberScreen | SelectLanguageScreen

eval :: Action -> WelcomeScreenState -> Eval Action ScreenOutput WelcomeScreenState
eval BackPressed state = do 
  case state.props.currentStage of
    _ | state.props.currentStage == DETECT_LOCATION || state.props.currentStage == ENABLE_LOCATION -> continue state --minimizeApp
    _ | state.props.currentStage == SELECT_CITY || state.props.currentStage == SELECT_LANG ||state.props.currentStage == CAROUSEL -> continue state{props{currentStage = DETECT_LOCATION}}
    _ -> continue state

eval (PrimaryButtonAC PrimaryButtonController.OnClick) state = do
  exit MobileNumberScreen

eval (MenuButtonAction (MenuButtonController.OnSelection btnState)) state = continue state { props { selectedLanguage = btnState.text.value }}

eval (ChangeStage newStage) state = continue state{props{currentStage = newStage}}

eval (GenericHeaderAC GenericHeaderController.PrefixImgOnClick) state = continue state{props{currentStage = DETECT_LOCATION}}

eval _ state = continue state