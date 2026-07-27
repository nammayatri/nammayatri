module Screens.AppUpdatePopUpScreen.ScreenData where

import Prelude
import Screens.Types (UpdatePopupType(..), AppUpdatePopUpScreenState)
import Screens.Types as ST
import Language.Strings (getString)
import Language.Types (STR(..))

initData :: AppUpdatePopUpScreenState
initData =
    {
        version : 1, 
        updatePopup : NoUpdatePopup,
        appUpdatedView : {
            primaryText : "App Update",
            secondaryText : "",
            optionTwoText : "Update",
            coverImageUrl : "",
            popupFlowType : ST.NORMAL
        }
    }