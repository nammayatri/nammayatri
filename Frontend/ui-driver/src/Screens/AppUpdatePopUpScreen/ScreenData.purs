module Screens.AppUpdatePopUpScreen.ScreenData where

import Prelude
import Screens.Types (PopupOverlayType(..), AppUpdatePopUpScreenState)
import Language.Strings (getString)
import Language.Types (STR(..))

initData :: AppUpdatePopUpScreenState
initData =
    {
        version : 1, 
        updatePopup : NoUpdate,
        appUpdatedView : {
            primaryText : "App Update",
            secondaryText : "",
            optionTwoText : getString UPDATE,
            coverImageUrl : ""
        }
    }