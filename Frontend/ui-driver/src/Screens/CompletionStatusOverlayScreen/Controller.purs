module Screens.CompletionStatusOverlayScreen.Controller where

import Screens.Types(CompletionStatusOverlayState)
import PrestoDOM (Eval, continue, exit,continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Prelude (class Show, pure, unit, bind)
import Prelude (($), (==))


instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ ->  pure unit

data ScreenOutput = RemoveScreen 

data Action = CountDown

eval :: Action -> CompletionStatusOverlayState -> Eval Action ScreenOutput CompletionStatusOverlayState
eval CountDown state = do
  exit $ RemoveScreen