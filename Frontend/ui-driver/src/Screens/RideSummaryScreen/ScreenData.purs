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
    termsOpen :: Boolean,
    exChargesOpen :: Boolean,
    inChargesOpen :: Boolean 
}

initData :: RideSummaryScreenState
initData = {
    data: {
    },
        
    props: {
        termsOpen : false,
        exChargesOpen : false,
        inChargesOpen : false
    }
}