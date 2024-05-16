module Screens.WelcomeScreen.Controller where
import Components.PrimaryButton.Controller as PrimaryButtonController
import JBridge (minimizeApp, firebaseLogEvent)
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, pure, ($), unit)
import PrestoDOM (Eval, update, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.Types (WelcomeScreenState)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "WelcomeScreen"
    BackPressed -> trackAppBackPress appId (getScreen HOME_SCREEN)
    PrimaryButtonAC act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen WELCOME_SCREEN) "primary_button" "onclick"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen WELCOME_SCREEN) "primary_button" "no_action"
    _ -> pure unit

data Action = BackPressed
            | AfterRender
            | PrimaryButtonAC PrimaryButtonController.Action
            | OnPageChanged String
            | ChangeSheet Int

data ScreenOutput = MobileNumberScreen

eval :: Action -> WelcomeScreenState -> Eval Action ScreenOutput WelcomeScreenState

eval BackPressed state = do 
    _ <- pure $ minimizeApp ""
    continue state

eval (PrimaryButtonAC PrimaryButtonController.OnClick) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_get_started"
  exit MobileNumberScreen

eval (OnPageChanged idx) state = continue state{data{currentActiveIndex = fromMaybe 0 $ fromString idx}}
eval (ChangeSheet idx) state = continue state{data{currentActiveIndex = idx}}

eval _ state = continue state