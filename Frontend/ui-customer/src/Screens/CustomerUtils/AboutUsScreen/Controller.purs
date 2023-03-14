module Screens.AboutUsScreen.Controller where

import Prelude (class Show, pure, unit, ($), discard)
import Screens.Types (AboutUsScreenState)
import PrestoDOM (Eval, continue, exit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen ABOUT_US_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen ABOUT_US_SCREEN)
      trackAppEndScreen appId (getScreen ABOUT_US_SCREEN)
    GenericHeaderActionController act -> case act of
      GenericHeaderController.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen ABOUT_US_SCREEN)
      GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "generic_header_action" "forward_icon"
    TermsAndConditions -> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "in_screen" "t_&_c"

data Action = GenericHeaderActionController GenericHeaderController.Action
            | BackPressed
            | TermsAndConditions
            | AfterRender

data ScreenOutput = GoToHomeScreen
eval :: Action -> AboutUsScreenState -> Eval Action ScreenOutput AboutUsScreenState

eval (GenericHeaderActionController (GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval BackPressed state = exit $ GoToHomeScreen

eval _ state = continue state