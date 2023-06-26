module Screens.AcknowledgementScreen.Controller where

import Components.PrimaryButton.Controller as PrimaryButtonController
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, bind, pure, ($), unit)
import PrestoDOM (Eval, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.Types (AcknowledgementScreenState)

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "AcknowledgementScreen"
    BackPressed -> trackAppBackPress appId (getScreen ACKNOWLEDGEMENT_SCREEN)
    PrimaryButtonAC act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen ACKNOWLEDGEMENT_SCREEN) "primary_button" "onclick"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen ACKNOWLEDGEMENT_SCREEN) "primary_button" "no_action"
    NoAction -> pure unit


data Action = BackPressed
            | AfterRender
            | NoAction
            | PrimaryButtonAC PrimaryButtonController.Action

data ScreenOutput = HomeScreen

eval :: Action -> AcknowledgementScreenState -> Eval Action ScreenOutput AcknowledgementScreenState
eval BackPressed state = continue state

eval (PrimaryButtonAC PrimaryButtonController.OnClick) state = exit HomeScreen

eval _ state = continue state