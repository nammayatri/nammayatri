module Screens.UploadParcelImageScreen.ScreenData where

import Prelude
import Data.Maybe
import Screens.Types
import Foreign.Object (empty)
import Screens.Types as ST

initData :: ST.UploadParcelImageScreenState
initData = {
    data: {
        rideId : "",
        imagePath : "",
        errorMessage : Nothing,
        imageId : ""
    },
    props: {
        showConfirmAndUploadButton : false,
        isStartRideActive : false,
        uploading : false
    }
}
