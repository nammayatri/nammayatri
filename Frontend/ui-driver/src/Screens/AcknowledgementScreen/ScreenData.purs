module Screens.AcknowledgementScreen.ScreenData where

import Screens.Types (AcknowledgementScreenState, IllustrationType(..))
import PrestoDOM as PrestoDOM
import Common.Types.App as Common
import Data.Maybe as Maybe

initData :: AcknowledgementScreenState
initData = {
    data: {
        illustrationAsset : "",
        title : Maybe.Nothing,
        description : Maybe.Nothing,
        primaryButtonText : Maybe.Nothing
    },
    props: {
        primaryButtonVisibility : PrestoDOM.VISIBLE,
        paymentStatus : Common.Success,
        illustrationType : Lottie
    }
 
}