module Screens.ExtraChargeInfoScreen.ScreenData where

import Prelude
import Services.API
import Data.Maybe


type ExtraChargeInfoScreenState = {
    optionOpened :: Array Boolean,
    sheetPositionRef :: Number,
    driverInfoResp :: Maybe GetDriverInfoResp
}


initData :: ExtraChargeInfoScreenState
initData = {
    optionOpened: [true, false],
    sheetPositionRef : 1.0,
    driverInfoResp : Nothing
}
