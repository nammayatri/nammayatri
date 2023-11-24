module Screens.AppUpdatePopUp.ScreenData where

import Prelude
import Screens.Types (UpdatePopupType(..), AppUpdatePopUpState)
import Language.Strings (getString)
import Language.Types (STR(..))
import Foreign.Object (empty)
import ConfigProvider
import Constants

initData :: AppUpdatePopUpState
initData =
    {
        version : 1, 
        logField : empty,
        updatePopup : NoUpdatePopup,
        appUpdatedView : {
            primaryText : "App Update",
            secondaryText : "",
            optionTwoText : getString UPDATE,
            coverImageUrl : ""
        },
        config : getAppConfig appConfig
    }