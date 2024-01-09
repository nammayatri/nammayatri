module PopUpOverlay.ScreenData where

import Prelude
import Language.Strings (getString)
import Language.Types (STR(..))
import Foreign.Object (empty)
import ConfigProvider
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Object (Object)
import MerchantConfig.Types
import PopUpOverlay.Types

initData :: PopUpOverlayState
initData =
    {
        version : 1, 
        logField : empty,
        updatePopup : NoUpdate,
        appUpdatedView : {
            primaryText : "App Update",
            secondaryText : "",
            optionTwoText : getString UPDATE,
            coverImageUrl : ""
        },
        config : getAppConfig appConfig
    }