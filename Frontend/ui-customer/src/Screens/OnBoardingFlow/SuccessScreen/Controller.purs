module Screens.SuccessScreen.Controller where

import Log (trackAppEndScreen, trackAppScreenRender)
import Prelude (class Show, ($))
import PrestoDOM (Eval, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (SuccessScreenState)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen SUCCESS_SCREEN)
    CountDown -> trackAppEndScreen appId (getScreen SUCCESS_SCREEN)

data ScreenOutput
  = RemoveScreen

data Action
  = CountDown
  | AfterRender

eval :: Action -> SuccessScreenState -> Eval Action ScreenOutput SuccessScreenState
eval CountDown state = do
  exit $ RemoveScreen

eval AfterRender state = continue state