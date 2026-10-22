module Screens.MetroWarriorsScreen.ScreenData where

import Screens.Types (MetroWarriorsScreenState)
import Data.Maybe as Mb
import Halogen.VDom.DOM.Prop (PropValue)
import Services.API as API 
import RemoteConfig as RU 
import ConfigProvider

initData :: MetroWarriorsScreenState
initData =
  { data: {
    config : getAppConfig appConfig,
    listItem : Mb.Nothing,
    stationList : [],
    searchString : Mb.Nothing,
    stationData : {
      primaryStation : Mb.Nothing,
      secondaryStationsData : [],
      isSpecialLocWarrior : false
    },
    remoteConfigData : RU.defaultMetroWarriorConfigEntity
  }
  , props: {
      showStationList : false,
      showShimmer : true
  }
  }

type StationListItem = {
  stationName :: String,
  id :: String
}

type StationListPropItem
  = { stationName :: PropValue
    , id :: PropValue
    }
  
type StationListCache = {
  list :: API.SpecialLocationListRes,
  cacheInvalidateCounter :: Int,
  city :: String
}