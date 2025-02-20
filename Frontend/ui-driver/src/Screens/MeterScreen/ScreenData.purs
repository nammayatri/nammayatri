module Screens.MeterScreen.ScreenData where

import Screens.Types (MeterScreenState, SearchLocationModelType(..), CustomerLocationSelectType(..))
import Data.Maybe as Mb
import Halogen.VDom.DOM.Prop (PropValue)
import Services.API as API 
import RemoteConfig as RU 
import ConfigProvider (getAppConfig, appConfig)
import PrestoDOM (Visibility(..))
import Data.Maybe (Maybe(..))
import Screens.Types (Address)
import Screens.BookingOptionsScreen.ScreenData as BOP
import JBridge (Location)

initData :: MeterScreenState
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
    sourceAddress: dummyAddress,
    destinationAddress: dummyAddress,
    locationList: [],
    savedlocationList: []
  }
  , props: {
      locateOnMap: false,
      sourceLat : 0.0,
      sourceLng : 0.0,
      destinationLat : 0.0,
      destinationLng : 0.0,
      sourcePlaceId : Nothing,
      currentLocation : dummyLocation,
      destinationPlaceId : Nothing,
      isSrcServiceable : true,
      searchType : Nothing,
      isDestServiceable : true,
      isSearchLocation: NoView,
      isRideServiceable: true,
      rideSearchProps : {
          sourceSelectType : SEARCH,
          sourceManuallyMoved : false,
          destManuallyMoved : false
      },
      sourceSetUsingPin: false,
      locateOnMapProps : { sourceLocationName : Nothing, sourceGeoJson : Nothing, sourceGates : Nothing, isSpecialPickUpGate : false, cameraAnimatedToSource : true },
      searchLocationModelProps: {
        isAutoComplete : false
       , showLoader : false
       , crossBtnSrcVisibility : false
       , crossBtnDestVisibility : false
      }
  }
  }

dummyAddress :: Address
dummyAddress = {
              "area" : Nothing
            , "state" : Nothing
            , "country" : Nothing
            , "building" : Nothing
            , "door" : Nothing
            , "street" : Nothing
            , "city" : Nothing
            , "areaCode" : Nothing
            , "ward" : Nothing
            , "placeId" : Nothing
            }

dummyLocation :: Location
dummyLocation = {
   place : "",
   lat : 0.0,
   lng : 0.0,
   address : Nothing,
   city : Nothing,
   isSpecialPickUp : Nothing
}
