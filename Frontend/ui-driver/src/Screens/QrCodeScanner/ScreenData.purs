module Screens.QrCodeScanner.ScreenData where

import Prelude
import Data.Maybe
import Screens.Types as ST

initData :: ST.QrCodeScannerState
initData = 
    {
        isError : false,
        qrData : ""
    }