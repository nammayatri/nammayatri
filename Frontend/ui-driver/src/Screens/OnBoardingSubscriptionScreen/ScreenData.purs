module Screens.OnBoardingSubscriptionScreen.ScreenData where

import Prelude
import Screens.Types (OnBoardingSubscriptionScreenState)
import Screens.SubscriptionScreen.ScreenData (dummyPlanConfig)
import Data.Maybe


initData :: OnBoardingSubscriptionScreenState
initData = {
    data:{
        plansList : [dummyPlanConfig],
        selectedPlanItem : Nothing
    },
    props:{
        isSelectedLangTamil : false,
        screenCount : 0
    }
}