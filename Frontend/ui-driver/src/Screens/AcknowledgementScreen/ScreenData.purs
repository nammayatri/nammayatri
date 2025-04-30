module Screens.AcknowledgementScreen.ScreenData where

import Domain.Payments as PP
import Data.Maybe as Maybe
import PrestoDOM as PrestoDOM
import Screens.Types (AcknowledgementScreenState, IllustrationType(..))

initData :: AcknowledgementScreenState
initData = {
    data: {
        illustrationAsset : "",
        title : Maybe.Nothing,
        description : Maybe.Nothing,
        primaryButtonText : Maybe.Nothing,
        orderId: Maybe.Nothing,
        amount : "",
        primaryButtonVisibility : true
    },
    props: {
        primaryButtonVisibility : PrestoDOM.VISIBLE,
        paymentStatus : PP.Success,
        illustrationType : Lottie
    }
 
}