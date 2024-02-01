module Screens.OnBoardingSubscriptionScreen.ScreenData where

import Data.Maybe
import Prelude

import ConfigProvider
import Screens.SubscriptionScreen.ScreenData (dummyPlanConfig)
import Screens.Types (OnBoardingSubscriptionScreenState)
import RemoteConfig as RC


initData :: OnBoardingSubscriptionScreenState
initData = {
    data:{
        plansList : [dummyPlanConfig],
        selectedPlanItem : Nothing,
        subscriptionConfig : (getAppConfig appConfig).subscriptionConfig,
        reelsData : RC.defaultReelsData
    },
    props:{
        isSelectedLangTamil : false,
        screenCount : 0,
        supportPopup : false,
        choosePlanSelected : false
    }
}