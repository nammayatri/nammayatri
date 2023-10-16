module Screens.OnBoardingSubscriptionScreen.ScreenData where

import Data.Maybe
import Prelude

import MerchantConfig.DefaultConfig as DC
import Screens.SubscriptionScreen.ScreenData (dummyPlanConfig)
import Screens.Types (OnBoardingSubscriptionScreenState)


initData :: OnBoardingSubscriptionScreenState
initData = {
    data:{
        plansList : [dummyPlanConfig],
        selectedPlanItem : Nothing,
        subscriptionConfig : DC.config.subscriptionConfig
    },
    props:{
        isSelectedLangTamil : false,
        screenCount : 0
    }
}