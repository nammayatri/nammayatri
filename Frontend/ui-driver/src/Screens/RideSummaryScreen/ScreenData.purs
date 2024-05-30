module Screens.RideSummaryScreen.ScreenData where

import Prelude
import Screens.Types as S


type RideSummaryScreenState = {
    data :: RideSummaryScreenData,
    props :: RideSummaryScreenProps
}


type RideSummaryScreenData = {
}

type RideSummaryScreenProps = {
    termsAndConditionOpen :: Boolean,
    excludedChargesOpen :: Boolean,
    includedChargesOpen :: Boolean, 
    pickUpOpen :: Boolean 
}

initData :: RideSummaryScreenState
initData = {
    data: {
    },
        
    props: {
        termsAndConditionOpen : false,
        excludedChargesOpen : false,
        includedChargesOpen : false,
        pickUpOpen : false
    }
}