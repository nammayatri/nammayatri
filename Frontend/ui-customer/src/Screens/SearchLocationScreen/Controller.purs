module Screens.SearchLocationScreen.Controller where

import Prelude
import PrestoDOM (Eval, continue, exit, continueWithCmd, updateAndExit)
import Screens.Types (SearchLocationScreenState, SearchLocationTextField(..), SearchLocationStage(..), LocationListItemState(..), GlobalProps, CardType(..))
import Components.LocationTagBarV2 as LocationTagBarController
import Components.LocationListItem as LocationListItemController
import Components.FavouriteLocationModel as FavouriteLocModelController
import Components.GenericHeader as GenericHeaderController
import Components.SaveFavouriteCard as SaveFavCardController 
import Components.PrimaryEditText as EditTextController
import Components.PrimaryButton as PrimaryButtonController
import Components.InputView as InputViewController
import Components.MenuButton as MenuButtonController
import Components.SavedLocationCard as SavedLocationCardController
import Components.PopUpModal as PopUpModalController
import Screens.SearchLocationScreen.ScreenData (dummyLocationInfo)
import PrestoDOM.Types.Core (class Loggable)
import Log (trackAppActionClick)
import Screens (getScreen, ScreenName(..))
import Data.String(length, trim, toLower, indexOf, Pattern(..), split, drop) as STR
import Data.Array (length, find, sortBy, filter, findIndex, head, nubByEq) as DA
import Debug (spy)
import JBridge (currentPosition, toast, hideKeyboardOnNavigation, updateInputString, locateOnMap, locateOnMapConfig, scrollViewFocus, showKeyboard, scrollViewFocus, animateCamera, hideKeyboardOnNavigation, exitLocateOnMap)
import Data.Maybe (fromMaybe, Maybe(..), isJust, maybe, isNothing ) as MB
import Data.Number (fromString) as NUM
import Helpers.Utils (updateLocListWithDistance, setText, getSavedLocationByTag)
import Data.Ord (comparing)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (runEffectFn1)
import Engineering.Helpers.Commons (getNewIDWithTag, isTrue)
import Resources.Constants (encodeAddress)

instance showAction :: Show Action where 
  show _ = ""

instance loggableAction :: Loggable Action where 
  performLog action appId = case action of 
    _ -> trackAppActionClick appId (getScreen SEARCH_LOCATION_SCREEN) "primary_button_action" "no_action"


data Action = NoAction 
            | MapReady String String String
            | AfterRender
            | InputChanged String
            | TextFieldFocusChanged SearchLocationTextField Boolean
            | UpdateLocAndLatLong (Array LocationListItemState) String String 
            | RecenterCurrentLocation 
            | SetLocationOnMap 
            | ClearTextField SearchLocationTextField
            | LocFromMap String String String
            | LocationTagBarAC (Array LocationListItemState) LocationTagBarController.Action
            | LocationListItemAC (Array LocationListItemState) LocationListItemController.Action 
            | FavouriteLocationModelAC FavouriteLocModelController.Action
            | SaveFavCardAC (Array LocationListItemState) SaveFavCardController.Action 
            | PrimaryButtonAC PrimaryButtonController.Action
            | InputViewAC GlobalProps InputViewController.Action 
            | MenuButtonAC MenuButtonController.Action
            | BackpressAction
            | PopUpModalAC PopUpModalController.Action
            | CurrentLocation 

data ScreenOutput = NoOutput  
                  | Reload SearchLocationScreenState
                  | SearchPlace String SearchLocationScreenState
                  | SaveFavLoc SearchLocationScreenState (Array LocationListItemState)
                  | ConfirmAndSaveFav SearchLocationScreenState
                  | AddFavLoc SearchLocationScreenState String
                  | PredictionClicked LocationListItemState SearchLocationScreenState
                  | UpdateLocName SearchLocationScreenState String String
                  | AddStop SearchLocationScreenState
                  | HomeScreen SearchLocationScreenState
                  | RentalsScreen SearchLocationScreenState
                  | LocSelectedOnMap SearchLocationScreenState

eval :: Action -> SearchLocationScreenState -> Eval Action ScreenOutput SearchLocationScreenState

eval (MapReady _ _ _) state = do 
  if state.props.searchLocStage == PredictionSelectedFromHome then 
    continueWithCmd state [do 
      pure (LocationListItemAC [] (LocationListItemController.OnClick state.data.predictionSelectedFromHome))
    ]
    else continue state
eval (MenuButtonAC (MenuButtonController.OnClick config)) state = do 
  continueWithCmd state{data{defaultGate = config.id}} [do
      let focusedIndex = DA.findIndex (\item -> item.place == config.id) state.data.nearByGates
      case focusedIndex of
        MB.Just index -> do
          _ <- pure $ scrollViewFocus (getNewIDWithTag "scrollViewParent") index
          pure unit
        MB.Nothing -> pure unit
      _ <- animateCamera config.lat config.lng 25.0 "NO_ZOOM"
      pure NoAction
    ]

eval (PrimaryButtonAC PrimaryButtonController.OnClick) state =  
  MB.maybe 
    (continue state) 
    (\ focussedField -> do
      let newState = if focussedField == SearchLocPickup then state{data{srcLoc = MB.Just state.data.latLonOnMap}}
                      else state{data{destLoc = MB.Just state.data.latLonOnMap}} 
      void $ pure $ exitLocateOnMap ""
      updateAndExit newState $ LocSelectedOnMap newState
    ) (state.props.focussedTextField)
 
eval CurrentLocation state = do
  let newState = state{data{srcLoc = state.data.currentLoc}, props{focussedTextField = MB.Just SearchLocDrop}}
  void $ pure $ showKeyboard $ getNewIDWithTag (show SearchLocDrop)
  updateAndExit newState $ Reload newState

eval (LocationListItemAC savedLocations (LocationListItemController.FavClick item) ) state = do 
  if (DA.length savedLocations >= 20) then do
    void $ pure $ toast ("SORRY_LIMIT_EXCEEDED_YOU_CANT_ADD_ANY_MORE_FAVOURITES")
    continue state
    else exit $ SaveFavLoc state{data{saveFavouriteCard{ address = item.description , selectedItem = item, tag = "", tagExists = false, isBtnActive = false }}} savedLocations

eval (LocationListItemAC _ (LocationListItemController.OnClick item)) state = do 
  void $ pure $ hideKeyboardOnNavigation true
  MB.maybe (continue state) (\currTextField -> predictionClicked currTextField ) state.props.focussedTextField
  where 
    predictionClicked currTextField = do 
      let updatedLoc = {placeId : item.placeId, address : item.description, lat : item.lat, lon : item.lon, city : MB.Nothing, addressComponents : LocationListItemController.dummyAddress}
          newState = if currTextField == SearchLocPickup then 
                      state { data { srcLoc = MB.Just updatedLoc }, props { isAutoComplete = false , canSelectFromFav = true}} 
                      else state { data { destLoc = MB.Just updatedLoc}, props {isAutoComplete = false, canSelectFromFav = true} }
      pure $ setText (getNewIDWithTag (show currTextField)) $ item.description
      updateAndExit newState $ PredictionClicked item newState

eval (InputViewAC globalProps (InputViewController.ClearTextField textField)) state = do 
  pure $ setText (getNewIDWithTag textField) $ ""
  continue state { data {locationList = fetchSortedCachedSearches state globalProps textField }
                 , props {canClearText = false, isAutoComplete = false, locUnserviceable = false}}
  
eval (InputViewAC _ (InputViewController.BackPressed)) state = handleBackPress state  

eval (BackpressAction) state = handleBackPress state 

eval (FavouriteLocationModelAC (FavouriteLocModelController.GenericHeaderAC (GenericHeaderController.PrefixImgOnClick))) state = continue state{props{searchLocStage = PredictionsStage}}

eval (FavouriteLocationModelAC (FavouriteLocModelController.FavouriteLocationAC (SavedLocationCardController.CardClicked item))) state = do 
  continueWithCmd state [do 
    pure (LocationListItemAC [] (LocationListItemController.OnClick item))
    ]

eval (LocationTagBarAC savedLoc (LocationTagBarController.TagClicked tag) ) state = do 
  case tag of 
    "ADD_HOME" ->  if DA.length savedLoc >= 20 then 
      continue state 
      else exit $ AddFavLoc state "HOME_TAG"
    "ADD_WORK" -> if DA.length savedLoc >= 20 then 
      continue state 
      else exit $ AddFavLoc state "WORK_TAG"
    "HOME" -> do 
      let homeLoc = MB.fromMaybe LocationListItemController.locationListStateObj $ getSavedLocationByTag savedLoc HOME_TAG
      continueWithCmd state [ do 
        pure (LocationListItemAC savedLoc (LocationListItemController.OnClick homeLoc))
      ]
    "WORK" -> do 
      let workLoc = MB.fromMaybe LocationListItemController.locationListStateObj $ getSavedLocationByTag savedLoc WORK_TAG
      continueWithCmd state [ do 
        pure (LocationListItemAC savedLoc (LocationListItemController.OnClick workLoc))
      ]
    _ -> do 
      void $ pure $ hideKeyboardOnNavigation true
      continue state{ props {searchLocStage = AllFavouritesStage}}

eval (InputViewAC globalProps (InputViewController.TextFieldFocusChanged textField isEditText hasFocus)) state = do
  if (state.props.searchLocStage == PredictionsStage || state.props.searchLocStage == LocateOnMapStage) then do
    let {srcLocation , destLocation} = mkSrcAndDestLoc textField
        setTextTo = (case textField of 
                      "SearchLocPickup" -> srcLocation
                      "SearchLocDrop" ->  destLocation
                      _ -> "")
    case textField of 
      "SearchLocPickup" -> pure $ setText (getNewIDWithTag "SearchLocDrop") $ destLocation
      "SearchLocDrop" -> pure $ setText (getNewIDWithTag "SearchLocPickup") $ srcLocation
      _ -> pure unit

    let canClear = STR.length setTextTo > 2
    let {pickUpLoc , dropLoc} =( case textField of 
                                  "SearchLocPickup" -> { pickUpLoc : state.props.textFieldText.pickUpLoc, dropLoc : ""}
                                  "SearchLocDrop" -> { pickUpLoc : "", dropLoc : state.props.textFieldText.dropLoc}
                                  _ -> {pickUpLoc : "", dropLoc : "" })
    let sortedCachedLoc = fetchSortedCachedSearches state globalProps textField
    let _ = spy "sortedCachedLoc" sortedCachedLoc
    continue state{ props{textFieldText {pickUpLoc = pickUpLoc , dropLoc = dropLoc}, focussedTextField = mkTextFieldTag textField , canClearText = canClear}
                  , data {locationList = sortedCachedLoc}}
    else continue state

  where
    mkTextFieldTag :: String -> MB.Maybe SearchLocationTextField
    mkTextFieldTag textField =
      case textField of 
        "SearchLocPickup" -> MB.Just SearchLocPickup
        "SearchLocDrop" -> MB.Just SearchLocDrop
        _ -> MB.Nothing

    mkSrcAndDestLoc :: String -> {srcLocation :: String, destLocation :: String}
    mkSrcAndDestLoc textField = 
      { srcLocation : MB.maybe "" (\srcLoc -> srcLoc.address) state.data.srcLoc
      , destLocation : MB.maybe "" (\destLoc -> destLoc.address) state.data.destLoc}
    
eval (InputViewAC _ (InputViewController.AutoCompleteCallBack value pickUpchanged)) state = do 
  let _ = spy "Inside AutoCompleteCallBack" "ABCD" 
  if state.props.isAutoComplete then -- so that selecting from favourites doesn't trigger autocomplete
    autoCompleteAPI state value $ if pickUpchanged then SearchLocPickup else SearchLocDrop
    else continue state

eval (InputViewAC _ (InputViewController.InputChanged value)) state = do 
  let canClearText = STR.length value > 2
  continueWithCmd state {props { canClearText = canClearText, isAutoComplete = canClearText}} [ do 
    void $ pure $ updateInputString value 
    pure NoAction
  ]
  
eval (UpdateLocAndLatLong recentSearches lat lng) state = do 
  let updatedLoc = {placeId : MB.Nothing, city : MB.Nothing, addressComponents : LocationListItemController.dummyAddress , address : "Current Location" , lat : NUM.fromString lat , lon : NUM.fromString lng}
      shouldUpdateCurrent = MB.maybe true (\loc -> (MB.fromMaybe 0.0 loc.lat) == 0.0) (state.data.currentLoc) 
      shouldUpdateSrc = MB.maybe true (\loc -> (MB.fromMaybe 0.0 loc.lat) == 0.0) (state.data.srcLoc)
  continue state{ data 
                    { srcLoc = if shouldUpdateSrc then MB.Just updatedLoc else state.data.srcLoc
                    , currentLoc = if shouldUpdateCurrent then MB.Just updatedLoc else state.data.currentLoc
                    , locationList = DA.sortBy (comparing (_.actualDistance)) $ updateLocListWithDistance recentSearches (MB.fromMaybe 0.0 updatedLoc.lat) (MB.fromMaybe 0.0 updatedLoc.lon) true state.appConfig.suggestedTripsAndLocationConfig.locationWithinXDist }
                    }

eval RecenterCurrentLocation state = continueWithCmd state [ do 
  void $ pure $ currentPosition "NO_ZOOM"
  pure NoAction 
]

eval (SaveFavCardAC _ (SaveFavCardController.OnClose)) state = 
  continue state { props {showSaveFavCard = false}
                 , data { saveFavouriteCard {isBtnActive = false, tag = "", tagExists = false, address = ""}}}

eval (SaveFavCardAC savedLoc (SaveFavCardController.PrimayEditTA (EditTextController.TextChanged id str))) state  = do 
  let input = STR.trim str
      tagExists = MB.isJust $ DA.find (\x -> (STR.toLower x.tag) == (STR.toLower input)) savedLoc
      updatedState = state{ data { saveFavouriteCard {tag = input, tagExists = tagExists , isBtnActive = (STR.length input) >= 3}}}
  continue updatedState

eval (SaveFavCardAC _ (SaveFavCardController.SaveFavourite)) state = do 
  void $ pure $ hideKeyboardOnNavigation true 
  exit $ ConfirmAndSaveFav state{ props{showSaveFavCard = false} }

eval (SetLocationOnMap) state = do 
  let { currentLat, currentLng } = 
        MB.maybe { currentLat: 0.0, currentLng: 0.0 } (\loc ->{ currentLat : MB.fromMaybe 0.0 loc.lat, currentLng: MB.fromMaybe 0.0 loc.lon }) (state.data.currentLoc)
      focussedField = MB.maybe MB.Nothing (\currField -> if currField == SearchLocPickup then (state.data.srcLoc) else  (state.data.destLoc)) (state.props.focussedTextField)
      { lat, lng } = 
        MB.maybe { lat : currentLat, lng : currentLng} (\loc -> mkLatLong currentLat currentLng loc) focussedField
  void $ pure $ hideKeyboardOnNavigation true
  void $ pure $ unsafePerformEffect $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = false, lat = lat, lon = lng, geoJson = "", points = [], zoomLevel = 17.0, labelId = getNewIDWithTag "LocateOnMapSLSPin"}
  let newState = state{props{searchLocStage = LocateOnMapStage, locUnserviceable = false}, data{latLonOnMap = MB.fromMaybe dummyLocationInfo focussedField }}
  updateAndExit newState $ Reload newState
  where 
    mkLatLong currentLat currentLng loc = { lat: MB.fromMaybe currentLat loc.lat, lng: MB.fromMaybe currentLng loc.lon }

eval (LocFromMap key lat lon) state = do
  case key of 
    "LatLon" -> do 
      let newState = state{data{defaultGate = ""}}
      updateAndExit newState $ UpdateLocName newState lat lon 
    _ -> do 
      let focusedIndex = DA.findIndex (\item -> item.place == key) state.data.nearByGates
      case focusedIndex of
        MB.Just index -> do
          void $ pure $ scrollViewFocus (getNewIDWithTag "scrollViewParentSLS") index
          exit $ UpdateLocName state{data{defaultGate = key}} lat lon
        MB.Nothing -> continue state
  
eval _ state = continue state


autoCompleteAPI state inputStr inputType = do
  case (not state.props.locUnserviceable) && (state.props.searchLocStage == PredictionsStage)  of 
    false -> continue state 
    true -> do 
      if (STR.length $ STR.trim inputStr) > 2 && inputStr /= "Current Location" then do 
        callSearchLocationAPI 
        else continue state{props{canClearText = false}}
  where 
    callSearchLocationAPI = do 
      let newState = state{props{ canSelectFromFav = false, showLoader = true, canClearText = true}}
      updateAndExit newState $ SearchPlace inputStr newState


fetchSortedCachedSearches state globalProps textField = do 
  let {currLat, currLon} = MB.maybe {currLat: 0.0, currLon: 0.0} (\currLoc -> {currLat: MB.fromMaybe 0.0 currLoc.lat, currLon: MB.fromMaybe 0.0 currLoc.lon}) state.data.currentLoc
      {srcLat, srcLon} = MB.maybe {srcLat: 0.0, srcLon: 0.0} (\srcLoc -> {srcLat: MB.fromMaybe 0.0 srcLoc.lat, srcLon: MB.fromMaybe 0.0 srcLoc.lon}) state.data.srcLoc
      {lat, lon} = if textField == "SearchLocPickup" then {lat: currLat, lon: currLon} else {lat: srcLat, lon: srcLon}
  removeDuplicates $ updateLocListWithDistance (globalProps.cachedSearches) lat lon true state.appConfig.suggestedTripsAndLocationConfig.locationWithinXDist

removeDuplicates :: Array LocationListItemState -> Array LocationListItemState
removeDuplicates arr = DA.nubByEq (\item1 item2 -> (item1.lat == item2.lat && item1.lon == item2.lon)) arr


handleBackPress state = do 
  case state.props.searchLocStage of 
    LocateOnMapStage -> do 
      void $ pure $ exitLocateOnMap ""
      continue state { data{ latLonOnMap = dummyLocationInfo }
      , props{ searchLocStage = PredictionsStage}}
    ConfirmLocationStage -> do 
      void $ pure $ exitLocateOnMap ""
      continue state {props {searchLocStage = PredictionsStage}, data{latLonOnMap = dummyLocationInfo}}
    PredictionsStage -> do 
      void $ pure $ hideKeyboardOnNavigation true
      if state.data.fromScreen == getScreen HOME_SCREEN then 
        exit $ HomeScreen state 
        else exit $ RentalsScreen state 
    _ -> continue state