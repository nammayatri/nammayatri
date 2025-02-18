module Screens.MeterMapScreen.ScreenData where

import ConfigProvider

import ConfigProvider (getAppConfig, appConfig)
import Data.Maybe as Mb
import Halogen.VDom.DOM.Prop (PropValue)
import PrestoDOM (Visibility(..))
import Prim.TypeError as True
import RemoteConfig as RU
import Screens.BookingOptionsScreen.ScreenData as BOP
import Screens.Types (MeterMapScreenState, SearchLocationModelType(..), CustomerLocationSelectType(..))
import Services.API as API
import Services.API (FarePolicyHour(..), ServiceTierType(..))
import Data.Maybe (Maybe(..))


initData :: MeterMapScreenState
initData =
  { data: {
    rateCard : BOP.dummyRateCard,
    listItem : Mb.Nothing,
    searchString : Mb.Nothing,
    isSearchLocation: NoView,
    appConfig: getAppConfig appConfig,
    isIntercityFlow : false,
    suffixButtonVisibility: GONE,
    startTimeUTC: "",
    isEditDestination : false,
    isSource: Mb.Just false,
    isDestViewEditable: true,
    source: "",
    destination: "",
    locationList: [],
    savedlocationList: []
  }
  , props: {
      enableOtpModal : false,
      enterOtpFocusIndex : 0,
      isCustomerNumberValid : false,
      alternateMobileOtp : "",
      otpAttemptsExceeded : false,
      otpIncorrect : false,
      customerMobileNumber : "",
      showRateCard: false,
      locateOnMap: false,
      isSearchLocation: NoView,
      isRideServiceable: true,
      rideSearchProps : {
          sourceSelectType : SEARCH
    },
    searchLocationModelProps: {
      isAutoComplete : false
     , showLoader : false
     , crossBtnSrcVisibility : false
     , crossBtnDestVisibility : false
    }
  }
  }
