module Screens.MeterMapScreen.ScreenData where

import Screens.Types (MeterMapScreenState, SearchLocationModelType(..), CustomerLocationSelectType(..))
import Data.Maybe as Mb
import Halogen.VDom.DOM.Prop (PropValue)
import Services.API as API 
import RemoteConfig as RU 
import ConfigProvider
import PrestoDOM (Visibility(..))

initData :: MeterMapScreenState
initData =
  { data: {
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
