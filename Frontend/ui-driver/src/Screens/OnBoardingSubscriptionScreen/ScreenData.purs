module Screens.OnBoardingSubscriptionScreen.ScreenData where

import Data.Maybe
import Prelude

import ConfigProvider
import Constants
import Screens.SubscriptionScreen.ScreenData (dummyPlanConfig)
import Screens.Types (OnBoardingSubscriptionScreenState)


initData :: OnBoardingSubscriptionScreenState
initData = {
    data:{
        plansList : [dummyPlanConfig],
        selectedPlanItem : Nothing,
        subscriptionConfig : (getAppConfig appConfig).subscriptionConfig
    },
    props:{
        isSelectedLangTamil : false,
        screenCount : 0
    }
}