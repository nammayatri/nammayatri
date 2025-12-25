module Screens.UploadParcelImageScreen.ScreenData where

import Prelude
import Data.Maybe
import Screens.Types
import Foreign.Object (empty)
import Screens.Types as ST
import ConfigProvider

initData :: ST.UploadParcelImageScreenState
initData = {
    data: {
        rideId : "",
        imagePath : "",
        errorMessage : Nothing,
        imageId : "",
        config : getAppConfig appConfig
    },
    props: {
        showConfirmAndUploadButton : false,
        isStartRideActive : false,
        uploading : false
    }
}
