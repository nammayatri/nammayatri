module Screens.WelcomeScreen.Controller where
import Components.PrimaryButton.Controller as PrimaryButtonController
import JBridge (minimizeApp, firebaseLogEvent)
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, pure, ($))
import PrestoDOM (Eval, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.Types (WelcomeScreenState)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "WelcomeScreen"
    BackPressed -> trackAppBackPress appId (getScreen HOME_SCREEN)
    PrimaryButtonAC act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen WELCOME_SCREEN) "primary_button" "onclick"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen WELCOME_SCREEN) "primary_button" "no_action"

data Action = BackPressed
            | AfterRender
            | PrimaryButtonAC PrimaryButtonController.Action

data ScreenOutput = MobileNumberScreen

eval :: Action -> WelcomeScreenState -> Eval Action ScreenOutput WelcomeScreenState
eval BackPressed state = do 
    _ <- pure $ minimizeApp ""
    continue state
eval (PrimaryButtonAC PrimaryButtonController.OnClick) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_get_started"
  exit MobileNumberScreen
eval _ state = continue state