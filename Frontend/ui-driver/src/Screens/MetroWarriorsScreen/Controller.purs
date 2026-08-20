module Screens.MetroWarriorsScreen.Controller where

import Components.PrimaryButton.Controller as PrimaryButtonController
import JBridge as JB
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude
import PrestoDOM
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.Types as ST
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import Components.GenericHeader as GenericHeader
import PrestoDOM.List as PList
import Data.Maybe as Mb
import Data.Array as DA
import Data.String as DS
import Services.API as API
import Storage (getValueToLocalStore, KeyStore(..))
import Data.Newtype (unwrap)
import RemoteConfig as RU
import Components.SwitchButtonView as SwitchButtonView

instance showAction :: Show Action where
  show (BackPressed) = "BackPressed"
  show (AfterRender) = "AfterRender"
  show (PrimaryButtonAC var1) = "PrimaryButtonAC_" <> show var1
  show (GenericHeaderAction var1) = "GenericHeaderAction_" <> show var1
  show (StationSearchTextChanged _) = "StationSearchTextChanged"
  show (SetListItem _) = "SetListItem"
  show (UpdateList _) = "UpdateList"
  show (SwitchToListView) = "SwitchToListView"
  show (RefreshDataWithChanges _ _ _) = "RefreshDataWithChanges"
  show (UpdateWarriorResp _) = "UpdateWarriorResp"
  show (NoAction) = "NoAction"
  show (OnStationClick _) = "OnStationClick"
  show (LearnMore) = "LearnMore"
  show (SwitchButtonAction var1) = "SwitchButtonAction_" <> show var1

instance loggableAction :: Loggable Action where
  performLog _ _ = pure unit

data Action
  = BackPressed
  | AfterRender
  | PrimaryButtonAC PrimaryButtonController.Action
  | GenericHeaderAction GenericHeader.Action
  | StationSearchTextChanged String
  | SetListItem PList.ListItem
  | UpdateList (Array API.SpecialLocation)
  | SwitchToListView
  | RefreshDataWithChanges Boolean (Mb.Maybe API.SpecialLocationWarrior) (Array String)
  | UpdateWarriorResp ST.MetroWarriorData
  | NoAction
  | OnStationClick Int
  | LearnMore
  | SwitchButtonAction SwitchButtonView.Action

data ScreenOutput
  = GoToHomeScreen ST.MetroWarriorsScreenState
  | UpdateWarriorSettings ST.MetroWarriorsScreenState API.UpdateSpecialLocWarriorInfoReq

eval :: Action -> ST.MetroWarriorsScreenState -> Eval Action ScreenOutput ST.MetroWarriorsScreenState

eval BackPressed state = do
  void $ pure $ JB.hideKeyboardOnNavigation true
  if state.props.showStationList then
    continue state { data{searchString = Mb.Nothing}, props { showStationList = false } }
  else
    exit $ GoToHomeScreen state{ data{searchString = Mb.Nothing}}

eval (SetListItem item) state = continue state { data { listItem = Mb.Just item } }

eval (UpdateWarriorResp warriorData) state = continue state { data { stationData = warriorData }, props {showShimmer = false} }

eval (StationSearchTextChanged newString) state = continue state { data { searchString = Mb.Just newString } }

eval (UpdateList list) state = continue state { data { stationList = DA.sortBy (\(API.SpecialLocation s1) (API.SpecialLocation s2) -> compare s1.locationName s2.locationName) list } }

eval SwitchToListView state = continue state { props { showStationList = true } }

eval (RefreshDataWithChanges toggle pStation secondaryStations) state = do
  let
    primaryStationId =
      Mb.maybe Mb.Nothing
        ( \(API.SpecialLocationWarrior primaryStation) ->
            Mb.Just primaryStation.id
        )
        pStation
  void $ pure $ JB.hideKeyboardOnNavigation true
  exit
    $ UpdateWarriorSettings state{props{showStationList = false}, data{searchString = Mb.Nothing}}
    $ API.UpdateSpecialLocWarriorInfoReq
        { isSpecialLocWarrior: toggle
        , preferredPrimarySpecialLocId: primaryStationId
        , preferredSecondarySpecialLocIds: secondaryStations
        , driverId: getValueToLocalStore DRIVER_ID
        }

eval (OnStationClick index) state = do
  let stationList = DA.filter (\(API.SpecialLocation item) -> DS.contains (DS.Pattern (DS.toLower $ Mb.fromMaybe "" state.data.searchString)) (DS.toLower item.locationName)) state.data.stationList
  continueWithCmd state [pure $ RefreshDataWithChanges true ((transformSpecialLocationToWarrior []) <$> (stationList DA.!! index)) []]

eval (GenericHeaderAction (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [pure BackPressed]

eval LearnMore state = do
  continueWithCmd state [ do
    void $ JB.openUrlInApp state.data.remoteConfigData.videoUrl
    pure NoAction
  ]

eval _ state = update state

transformSpecialLocationToWarrior :: Array API.SpecialLocation -> API.SpecialLocation -> API.SpecialLocationWarrior
transformSpecialLocationToWarrior linkedLocations (API.SpecialLocation station) =
  API.SpecialLocationWarrior
    { id: station.id
    , locationName: station.locationName
    , category: station.category
    , linkedLocations: linkedLocations
    }

getMetroWarriorFromLocationId :: Array API.SpecialLocation -> String -> Mb.Maybe API.SpecialLocationWarrior
getMetroWarriorFromLocationId stationList id = (transformSpecialLocationToWarrior []) <$> DA.find (\(API.SpecialLocation item) -> item.id == id) stationList

getMetroWarriorFromName :: Array API.SpecialLocation -> Array API.SpecialLocation -> String -> Mb.Maybe API.SpecialLocationWarrior
getMetroWarriorFromName stationList linkedLocations name = transformSpecialLocationToWarrior linkedLocations <$> DA.find (\(API.SpecialLocation item) -> item.locationName == name) stationList

getSpecialLocationFromName :: Array API.SpecialLocation -> String -> Mb.Maybe API.SpecialLocation
getSpecialLocationFromName stationList name = DA.find (\(API.SpecialLocation item) -> item.locationName == name) stationList

getStationId :: API.SpecialLocationWarrior -> String
getStationId station = (unwrap station).id

makeStationsData :: API.SpecialLocWarriorInfoRes -> Array API.SpecialLocation -> RU.MetroWarriorConfigEntity -> ST.MetroWarriorData
makeStationsData (API.SpecialLocWarriorInfoRes settings) stations config = do
  let defaultLocationList = DA.mapMaybe identity $ (getSpecialLocationFromName stations) <$> config.defaultSecondaryStations
  { primaryStation: Mb.maybe (getMetroWarriorFromLocationId stations config.defaultPrimaryStation) (\_ -> settings.preferredPrimarySpecialLoc) settings.preferredPrimarySpecialLoc
  , secondaryStationsData: Mb.maybe (getStationId <$> (transformSpecialLocationToWarrior []) <$> defaultLocationList) (\_ -> settings.preferredSecondarySpecialLocIds) settings.preferredPrimarySpecialLoc
  , isSpecialLocWarrior: settings.isSpecialLocWarrior
  }