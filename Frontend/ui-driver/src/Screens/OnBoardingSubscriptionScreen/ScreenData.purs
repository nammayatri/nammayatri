module Screens.OnBoardingSubscriptionScreen.ScreenData where

import Data.Maybe
import Prelude

import ConfigProvider
import Screens.SubscriptionScreen.ScreenData (dummyPlanConfig)
import Screens.Types (OnBoardingSubscriptionScreenState)
import RemoteConfig as RC
import Common.RemoteConfig.Utils as CommonRC


initData :: OnBoardingSubscriptionScreenState
initData = {
    data:{
        config : getAppConfig appConfig,
        plansList : [dummyPlanConfig],
        selectedPlanItem : Nothing,
        subscriptionConfig : (getAppConfig appConfig).subscriptionConfig,
        reelsData : RC.defaultReelsData,
        vehicleCategory : Nothing,
        freeTrialDays : Nothing,
        freeTrialRides : Nothing,
        totalRidesTaken : Nothing,
        vehicleAndCityConfig : CommonRC.defaultSubscriptionsConfigVariantLevelEntity
    },
    props:{
        isSelectedLangTamil : false,
        screenCount : 0,
        supportPopup : false,
        choosePlanSelected : false
    }
}