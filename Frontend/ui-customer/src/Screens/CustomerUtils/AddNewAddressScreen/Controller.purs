{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AddNewAddressScreen.Controller where

import Prelude
import Accessor (_description, _place_id, _distance)
import Common.Types.App (LazyCheck (..))
import Components.GenericHeader as GenericHeader
import Components.LocationListItem as LocationListItemController
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Array ((!!), length, filter, any, sortBy, null, head) as DA
import Data.Lens ((^.))
import Data.Ord
import Data.Eq
import Helpers.Utils (parseFloat)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, fromJust, isJust)
import Data.Number (fromString) as Number
import Data.String (trim, length, split, Pattern(..), drop, indexOf, toLower)
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (os, getNewIDWithTag)
import Engineering.Helpers.Utils (showToast)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Helpers.Utils (getCurrentLocationMarker, getDistanceBwCordinates, getLocationName)
import JBridge (animateCamera, currentPosition, exitLocateOnMap, hideKeyboardOnNavigation, isLocationEnabled, isLocationPermissionEnabled, locateOnMap, removeAllPolylines, requestKeyboardShow, requestLocation, toast, toggleBtnLoader, firebaseLogEvent, locateOnMapConfig)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, Ordering, Unit, bind, compare, discard, map, not, pure, unit, void, show, ($), (&&), (+), (-), (/=), (<>), (==), (>), (>=), (||))
import PrestoDOM (Eval, update, Visibility(..), continue, exit, updateAndExit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Resources.Constants (DecodeAddress(..), decodeAddress, getAddressFromSaved, getValueByComponent, getWard)
import Screens (ScreenName(..), getScreen)
import Screens.HomeScreen.ScreenData (dummyAddress)
import Screens.HomeScreen.Transformer (checkShowDistance)
import Screens.Types (AddNewAddressScreenState, CardType(..), LocationListItemState, DistInfo, LocItemType(..), LocationItemType(..))
import Services.API (AddressComponents, Prediction, SavedReqLocationAPIEntity(..))
import Storage (KeyStore(..), getValueToLocalStore)
import JBridge (fromMetersToKm, Location)
import Helpers.Utils (emitTerminateApp, isParentView)
import Common.Resources.Constants (pickupZoomLevel)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen ADD_NEW_ADDRESS_SCREEN)
    BackPressed backpressState -> pure unit --do // TODO:: FIX THIS ISSUE OF UNDEFINED
      -- _ <- pure $ spy "IN CONTROLLER" backpressState
      -- -- trackAppBackPress appId (getScreen ADD_NEW_ADDRESS_SCREEN)
      -- case backpressState.props.isLocateOnMap , backpressState.props.editLocation , backpressState.props.showSavePlaceView , backpressState.props.fromHome of
      --   true , _ , _ , _ -> trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "backpress_in_locate_on_map"
      --   _ , true , false , _ -> trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "backpress_in_edit_location"
      --   _ , _ , _ , true -> do
      --     trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "backpress_in_from_home"
      --     trackAppEndScreen appId (getScreen ADD_NEW_ADDRESS_SCREEN)
      --   _ , _ , _ , _ -> do
      --     trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "backpress_in_exit_locate_on_map"
      --     trackAppEndScreen appId (getScreen ADD_NEW_ADDRESS_SCREEN)
    GenericHeaderAC act -> case act of
      GenericHeader.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen ADD_NEW_ADDRESS_SCREEN)
      GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "generic_header_action" "forward_icon"
    LocationListItemAC act -> case act of
      LocationListItemController.SelectedCurrentLocation lat lon name -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "location_list_item" "current_location"
      LocationListItemController.OnClick item -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "location_list_item" "location"
      LocationListItemController.FavClick act -> trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "location_list_item" "fav"
    PrimaryEditTextAC act -> case act of
      PrimaryEditText.TextChanged _ _ -> trackAppTextInput appId (getScreen ADD_NEW_ADDRESS_SCREEN)"save_as_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged _ -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "primary_button" "focus_changed"
      PrimaryEditText.TextImageClicked -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "primary_button" "text_image_clicked"
    PrimaryButtonAC act -> case act of
      PrimaryButton.OnClick -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "primary_button" "onclick"
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "primary_button" "no_action"
    PrimaryButtonConfirmLocAC act -> case act of
      PrimaryButton.OnClick -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "primary_button" "confirm_loc"
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "primary_button" "no_action"
    AddressChanged input -> trackAppTextInput appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "address_text_changed"
    TagSelected index -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "favourite_tag"
    ChangeAddress -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "change_address"
    UpdateCurrentLocation lat lon ->  trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "current_location_updated"
    RecenterCurrentLocation -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "recenter"
    SetLocationOnMap -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "set_loc_on_map"
    ClearEditText -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "editted_text_onclear"
    SelectedCurrentLocation key lat lon ->  trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "current_location_selected"
    MAPREADY key latitude longitude -> trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "map_view_rendered"
    UpdateLocation key lat lon -> trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "location_updated"
    EditTextFocusChanged -> trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "edit_text_change_focussed"
    UpdateCurrLocName lat lon name -> trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "current_location_name_updated"
    CurrentLocationAction -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "current_location"
    RenderKeyboardActin -> trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "render_keyboard"
    NoAction -> trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "no_action"


data ScreenOutput = SearchPlace String AddNewAddressScreenState
                  | AddLocation AddNewAddressScreenState
                  | UpdateLocationName AddNewAddressScreenState Number Number
                  | GoToFavourites
                  | CheckLocServiceability AddNewAddressScreenState LocItemType
                  | GoToHome
                  | GoToSearchLocScreen

data Action = BackPressed AddNewAddressScreenState
            | NoAction
            | GenericHeaderAC GenericHeader.Action
            | LocationListItemAC LocationListItemController.Action
            | PrimaryEditTextAC PrimaryEditText.Action
            | PrimaryButtonAC PrimaryButton.Action
            | PrimaryButtonConfirmLocAC PrimaryButton.Action
            | AddressChanged String
            | TagSelected Int
            | ChangeAddress
            | SetLocationOnMap
            | MAPREADY String String String
            | AfterRender
            | UpdateLocation String String String
            | UpdateCurrLocName String String String
            | UpdateCurrentLocation String String
            | RecenterCurrentLocation
            | SelectedCurrentLocation Number Number String
            | EditTextFocusChanged
            | ClearEditText
            | CurrentLocationAction
            | RenderKeyboardActin

eval :: Action -> AddNewAddressScreenState -> Eval Action ScreenOutput AddNewAddressScreenState

eval (AddressChanged input) state = do
  if input /= state.data.address then do
    if (getLocTag (fromMaybe "" state.data.selectedItem.tagType)) ==  (Just CURR_LOC) then continue state{data{selectedItem{tag = ""}}, props{isLocationServiceable = if( not state.props.isLocationServiceable) then  (input /= state.data.address) else true}}
      else do
        if (length input > 2) then do
          validateSearchInput state input
          else continue state{props{isLocationServiceable = (input /= state.data.address)}}
    else continue state

eval (MAPREADY key latitude longitude) state = do 
    continueWithCmd state[ do
      _ <- checkPermissionAndUpdatePersonMarker state
      pure AfterRender
    ]

eval (ClearEditText) state = do
  void $ pure $ requestKeyboardShow (getNewIDWithTag "SavedLocationEditText")
  continue state{props{isSearchedLocationServiceable = true}}

eval SetLocationOnMap state = do
  let _ = unsafePerformEffect $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = true, lat = 0.0, lon = 0.0, geoJson = state.data.polygonCoordinates, points = state.data.nearByPickUpPoints, zoomLevel = pickupZoomLevel, labelId = getNewIDWithTag "AddAddressPin"}
  _ <- pure $ hideKeyboardOnNavigation true
  _ <- pure $ toggleBtnLoader "" false
  _ <- pure $ firebaseLogEvent "ny_user_favourite_select_on_map"
  let newState = state{props{isLocateOnMap = true}}
  continue newState

eval (UpdateLocation key lat lon) state = do
  let latitude = fromMaybe 0.0 (Number.fromString lat)
      longitude = fromMaybe 0.0 (Number.fromString lon)
  case key of
    "LatLon" -> do
      let selectedSpot = DA.head (DA.filter (\spots -> (getDistanceBwCordinates latitude longitude spots.lat spots.lng) * 1000.0 < 1.0 ) state.data.nearByPickUpPoints)
      exit $ UpdateLocationName state{props{defaultPickUpPoint = ""}} latitude longitude
    _ ->  case DA.head (DA.filter(\item -> item.place == key) state.data.nearByPickUpPoints) of
            Just spot -> exit $ UpdateLocationName state{props{defaultPickUpPoint = key}} spot.lat spot.lng
            Nothing -> continue state

eval (UpdateCurrLocName lat lon name) state = do
  continue state {data{locSelectedFromMap = name, latSelectedFromMap = (fromMaybe 0.0 (Number.fromString lat)), lonSelectedFromMap = (fromMaybe 0.0 (Number.fromString lon))}}

eval (SelectedCurrentLocation lat lon name) state = do
  void $ pure $ hideKeyboardOnNavigation true
  exit $ CheckLocServiceability (state{data{address = name, locationList = [] , selectedItem{tag = "Current_Location",lat = Just lat, lon = Just lon, placeId = Nothing, locationItemType = Nothing, description = name,title =(fromMaybe "" ((split (Pattern "," ) name) DA.!! 0) ), subTitle = (drop ((fromMaybe 0 (indexOf (Pattern ",") (name))) + 2) (name))  }}}) (CURR_LOC)

eval (UpdateCurrentLocation lat lng) state = continue state{data{ lat = (fromMaybe 0.0 (Number.fromString lat) ), lon = (fromMaybe 0.0 (Number.fromString lng))}}

eval RecenterCurrentLocation state = continueWithCmd state [do
  _ <- pure $ currentPosition "NO_ZOOM"
  pure NoAction
  ]

eval (BackPressed backpressState) state = do
  let searchLocScreen = getScreen SEARCH_LOCATION_SCREEN
      homeScreen = getScreen HOME_SCREEN
  case state.props.isLocateOnMap , state.props.editLocation , state.props.showSavePlaceView , state.props.fromHome  of
    true , _ , _ , _ -> do
      continue state{props{isLocateOnMap = false}, data{selectedItem{description = state.data.address},locationList = state.data.recentSearchs.predictionArray}}
    _ , true , false , _ -> do
        if state.data.activeIndex == Just 2 then pure unit
          else void $ pure $ hideKeyboardOnNavigation true
        continue state {props{showSavePlaceView = true, isLocateOnMap = false}, data{address= state.data.selectedItem.description}}
    _ , _ , _ , true -> do
      exit $ GoToHome
    _ , _ , _ , false -> do 
      if (state.props.fromScreen == searchLocScreen) then
        if isParentView FunctionCall 
        then do 
          void $ pure $ emitTerminateApp Nothing true
          continue state
        else exit $ GoToSearchLocScreen
      else do 
        void $ pure $ hideKeyboardOnNavigation true
        void $ pure $ exitLocateOnMap ""
        updateAndExit state{props{editLocation = false}} $ GoToFavourites

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure $ BackPressed state]

eval (LocationListItemAC (LocationListItemController.SelectedCurrentLocation lat lon name)) state = do
  let newState = state{data{ address = name ,selectedItem{lat = Just lat, lon = Just lon, description = name, placeId = Nothing }}}
  updateAndExit newState $  CheckLocServiceability (state{data{address= name , selectedItem{lat = Just lat, lon = Just lon, placeId = Nothing,locationItemType = Nothing, description = name}}}) (CURR_LOC)

eval (LocationListItemAC (LocationListItemController.OnClick item))  state = do
  if item.isClickable then do
    case (state.data.activeIndex ) of
      Just 2 -> void $ pure $ requestKeyboardShow (getNewIDWithTag "SaveAsEditText")
      _      -> void $ pure $ hideKeyboardOnNavigation true
    case (getLocTag (fromMaybe "" item.tagType)) of
      Just tagType  -> case tagType of
            LOCATE_ON_MAP -> continueWithCmd state [pure $ SetLocationOnMap]
            LOC_LIST      -> exit $ CheckLocServiceability state{data{selectedItem = item, address = item.description}} (LOC_LIST)
            _      -> continue state
      Nothing       -> continue state
    else do
      void $ pure $ showToast (getString LOCATION_ALREADY_EXISTS)
      continue state

eval (PrimaryButtonConfirmLocAC (PrimaryButton.OnClick)) state = do
  _ <- pure $ exitLocateOnMap ""
  exit $ CheckLocServiceability (state{data { address = state.data.locSelectedFromMap
                                            , selectedItem{placeId = Nothing
                                            , description = state.data.locSelectedFromMap
                                            , title = (fromMaybe "" ((split (Pattern "," ) state.data.locSelectedFromMap)DA.!! 0) )
                                            , subTitle = (drop ((fromMaybe 0 (indexOf (Pattern ",") (state.data.locSelectedFromMap))) + 2) (state.data.locSelectedFromMap))
                                            , lat = Just state.data.latSelectedFromMap
                                            , lon = Just state.data.lonSelectedFromMap 
                                            , locationItemType = Nothing }
                                            }
                                          }) (LOCATE_ON_MAP)

eval (TagSelected index) state = do
  void $ pure $ requestKeyboardShow (getNewIDWithTag "SaveAsEditText")
  let activeTag = case index of
                    0 -> "Home"
                    1 -> "Work"
                    _ -> state.data.addressSavedAs
  if (activeTag == "") then  continue state{props{isBtnActive = false}, data {activeIndex = Just index,  selectedTag = getTag index }}
    else
      if (validTag state.data.savedTags activeTag state.data.placeName) then
        continue state{ data  { activeIndex = Just index
                              , selectedTag = getTag index
                              , addressSavedAs = state.data.addressSavedAs}
                      , props { placeNameExists = state.props.placeNameExists 
                              , isBtnActive = (state.data.addressSavedAs /= "") || (state.props.editLocation && state.data.placeName /="" )}}
        else do
          void $ pure $ showToast ((case (toLower activeTag) of
                                  "home" -> (getString HOME)
                                  "work" -> (getString WORK)
                                  _      -> "") <> " " <> (getString LOCATION_ALREADY_EXISTS))
          continue state{data{addressSavedAs = state.data.addressSavedAs }, props {placeNameExists = state.props.placeNameExists}}

eval (ChangeAddress ) state = do
  continue state{props{showSavePlaceView = false, editLocation = true, isSearchedLocationServiceable = state.props.isLocationServiceable},data{latSelectedFromMap = state.data.lat , lonSelectedFromMap = state.data.lon ,locationList= state.data.recentSearchs.predictionArray}}

eval (PrimaryEditTextAC (PrimaryEditText.TextChanged id input)) state = do
  case (trim input) of
    "" ->  continue state{props{isBtnActive = false, placeNameExists = false}, data{addressSavedAs = ""}}
    _  -> if (validTag state.data.savedTags (trim input) state.data.placeName) then
            if (length (trim input) >= 3 ) then continue state {data{addressSavedAs =(trim input)},props{isBtnActive = if (state.data.selectedTag /= Nothing) then true else false, placeNameExists = false}}
              else continue state {data{addressSavedAs = ""}, props{isBtnActive = isJust state.data.selectedTag, placeNameExists = false}}
            else continue state{props{isBtnActive = false, placeNameExists = true},data{ addressSavedAs = (trim input) }}

eval (PrimaryButtonAC (PrimaryButton.OnClick)) state = do
  void $ pure $ hideKeyboardOnNavigation true
  updateAndExit state $ AddLocation state
eval _ state = update state

validateSearchInput :: AddNewAddressScreenState -> String -> Eval Action ScreenOutput AddNewAddressScreenState
validateSearchInput state searchString =
  if  length (trim searchString) > 2 then
        callSearchLocationAPI
  else continue state { props { isLocationServiceable = if (not state.props.isLocationServiceable)
                                                          then (searchString /= state.data.address)
                                                          else state.props.isLocationServiceable, isSearchedLocationServiceable = true  } }
  where
  callSearchLocationAPI = updateAndExit state { props { showSavePlaceView = false
                                                      , isLocationServiceable = if (not state.props.isLocationServiceable )
                                                                                  then (searchString /= state.data.address)
                                                                                  else state.props.isLocationServiceable, isSearchedLocationServiceable = true  }} $ SearchPlace searchString state{props{showSavePlaceView = false, isSearchedLocationServiceable = true,isLocationServiceable = if (not state.props.isLocationServiceable ) then (searchString /= state.data.address) else true}}

getLocationList :: Array Prediction -> Array LocationListItemState
getLocationList prediction = map (\x -> getLocation x) prediction

getLocation :: Prediction -> LocationListItemState
getLocation prediction = {
    postfixImageUrl : " "
  , prefixImageUrl : fetchImage FF_ASSET "ny_ic_loc_grey"
  , postfixImageVisibility : false
  , title : (fromMaybe "" ((split (Pattern ",") (prediction ^. _description)) DA.!! 0))
  , subTitle : (drop ((fromMaybe 0 (indexOf (Pattern ",") (prediction ^. _description))) + 2) (prediction ^. _description))
  , placeId : prediction ^._place_id
  , lat : Nothing
  , lon : Nothing
  , description : prediction ^. _description
  , tag : ""
  , tagType : Just $ show LOC_LIST
  , cardType : Nothing
  , address : ""
  , tagName : ""
  , isEditEnabled : true
  , savedLocation : ""
  , placeName : ""
  , isClickable : true
  , alpha : 1.0
  , fullAddress : dummyAddress
  , locationItemType : Just PREDICTION
  , distance : Just (fromMetersToKm (fromMaybe 0 (prediction ^. _distance)))
  , showDistance : Just $ checkShowDistance (fromMaybe 0 (prediction ^. _distance))
  , actualDistance : (prediction ^. _distance)
  , frequencyCount : Nothing
  , recencyDate : Nothing
  , locationScore : Nothing
  , dynamicAction : Nothing
  , types : Nothing
}

encodeAddressDescription :: AddNewAddressScreenState -> SavedReqLocationAPIEntity
encodeAddressDescription state = do
    let totalAddressComponents = DA.length $ split (Pattern ", ") state.data.selectedItem.description
        splitedAddress = split (Pattern ", ") state.data.selectedItem.description
        addressComponents = state.data.addressComponents
    SavedReqLocationAPIEntity{
                    "area": (splitedAddress DA.!!(totalAddressComponents-4) ) <> Just " ",
                    "areaCode": Nothing ,
                    "building": (splitedAddress DA.!!(totalAddressComponents-6) ) <> Just " ",
                    "city": (splitedAddress DA.!!(totalAddressComponents-3) ) <> Just " ",
                    "country": (splitedAddress DA.!!(totalAddressComponents-1) ) <> Just " ",
                    "state" : (splitedAddress DA.!!(totalAddressComponents-2) ) <> Just " ",
                    "door": if totalAddressComponents > 7  then (splitedAddress DA.!!0 ) <> Just " " <>(splitedAddress DA.!!1) <> Just "" else if totalAddressComponents == 7 then (splitedAddress DA.!!0 ) <> Just " " else  Just "",
                    "street": (splitedAddress DA.!!(totalAddressComponents-5) ) <> Just "",
                    "lat" : (fromMaybe 0.0 state.data.selectedItem.lat),
                    "lon" : (fromMaybe 0.0 state.data.selectedItem.lon),
                    "tag" : case state.data.selectedTag of
                              Just HOME_TAG -> "Home"
                              Just WORK_TAG -> "Work"
                              Just OTHER_TAG -> state.data.addressSavedAs
                              Nothing  -> state.data.addressSavedAs,
                    "placeId" : state.data.selectedItem.placeId,
                    "ward" : if DA.null addressComponents then
                        getWard Nothing (splitedAddress DA.!! (totalAddressComponents - 4)) (splitedAddress DA.!! (totalAddressComponents - 5)) (splitedAddress DA.!! (totalAddressComponents - 6))
                      else
                        Just $ getValueByComponent addressComponents "sublocality",
                    "locationName" : Just state.data.addressSavedAs
                }

getSavedLocations :: (Array SavedReqLocationAPIEntity) -> Array LocationListItemState
getSavedLocations savedLocation =  (map (\ (SavedReqLocationAPIEntity item) ->
  {
  prefixImageUrl : fetchImage FF_ASSET $ case (toLower (item.tag) ) of 
                "home" -> "ny_ic_home_blue"
                "work" -> "ny_ic_work_blue"
                _      -> "ny_ic_fav_red"
, postfixImageUrl : ""
, postfixImageVisibility : false
, title : (fromMaybe "" ((split (Pattern ",") (decodeAddress(SavedLoc (SavedReqLocationAPIEntity item)))) DA.!! 0))
, subTitle : (drop ((fromMaybe 0 (indexOf (Pattern ",") (decodeAddress (SavedLoc (SavedReqLocationAPIEntity item))))) + 2) (decodeAddress (SavedLoc (SavedReqLocationAPIEntity item))))
, lat : (Just item.lat)
, lon : (Just item.lon)
, description : (fromMaybe "" ((split (Pattern ":") (decodeAddress (SavedLoc (SavedReqLocationAPIEntity item)))) DA.!! 0))
, placeId : item.placeId
, tag : item.tag
, tagType : Just (show LOC_LIST)
, cardType : Nothing
, address : ""
, tagName : ""
, isEditEnabled : true
, savedLocation : ""
, placeName : ""
, isClickable : true
, alpha : 1.0
, fullAddress : getAddressFromSaved (SavedReqLocationAPIEntity item)
, locationItemType : Just SAVED_LOCATION
, distance : Nothing
, showDistance : Just false
, actualDistance : Nothing
, frequencyCount : Nothing
, recencyDate : Nothing
, locationScore : Nothing
, dynamicAction : Nothing
, types : Nothing 
}) savedLocation )

getSavedTags :: (Array SavedReqLocationAPIEntity) -> Array String
getSavedTags savedLocation = (map (\(SavedReqLocationAPIEntity item) -> toLower (item.tag) ) savedLocation)

getSavedTagsFromHome :: (Array LocationListItemState) -> Array String
getSavedTagsFromHome savedLocation = (map (\(item) -> toLower (item.tag)) savedLocation)


-- savedTagExists :: (Array LocationListItemState) -> String -> Boolean
-- savedTagExists savedLoc tag = 
--   (isJust find (\x -> (toLower x) == (toLower tag)) savedLoc)

validTag :: (Array String) -> String -> String -> Boolean
validTag savedTags input editTag = not (DA.any (\x -> (trim (toLower x)) == trim (toLower input)) (DA.filter (\item -> (trim (toLower item)) /= (trim (toLower editTag)) ) savedTags) )

getTag :: Int -> Maybe CardType
getTag index =  case index of
                  0 -> Just HOME_TAG
                  1 -> Just WORK_TAG
                  2 -> Just OTHER_TAG
                  _ -> Nothing

validateSelectedLocation :: LocationListItemState -> Array LocationListItemState -> Array LocationListItemState
validateSelectedLocation item savedLocations = ((DA.filter (\x ->  (x.placeId) == item.placeId)  savedLocations))

checkPermissionAndUpdatePersonMarker :: AddNewAddressScreenState -> Effect Unit
checkPermissionAndUpdatePersonMarker state = do
  conditionA <- isLocationPermissionEnabled unit
  conditionB <- isLocationEnabled unit
  let conditionC = (not state.props.isLocateOnMap)
  if (conditionA && conditionB && conditionC) then do
    _ <- getLocationName (showPersonMarker state (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))) 9.9 9.9 "Current Location" constructLatLong
    pure unit
    else do
      if (os == "IOS" && conditionC) then do
        _ <- getLocationName (showPersonMarker state (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))) 9.9 9.9 "Current Location" constructLatLong
        pure unit
      else if (not conditionA || not conditionB) then do
        _ <- requestLocation unit
        _ <- checkPermissionAndUpdatePersonMarker state
        pure unit
      else pure unit

showPersonMarker :: AddNewAddressScreenState -> String -> Location -> Effect Unit
showPersonMarker state marker location = animateCamera location.lat location.lng pickupZoomLevel "ZOOM"

constructLatLong :: Number -> Number -> String -> Location
constructLatLong lat lng _ =
  { lat: lat
  , lng : lng
  , place : ""
  , address : Nothing
  , city : Nothing
  , isSpecialPickUp : Nothing
  }

calculateDistance ::Array LocationListItemState -> String -> Number -> Number -> Array DistInfo
calculateDistance savedLocations excludeTag lat lon = do
  DA.sortBy compareByDistance ( map(\( item) -> (do
        let x = getDistanceBwCordinates  ( fromMaybe 0.0 item.lat) ( fromMaybe 0.0 item.lon) (lat) (lon)
        {locationName :item.tag , distanceDiff : x}
        )) (DA.filter (\x -> ((toLower x.tag) /= (toLower excludeTag))) savedLocations))

isValidLocation :: Array LocationListItemState -> String -> String -> Array DistInfo
isValidLocation savedLocations excludeTag placeId = do
 map (\item -> do
    {locationName : item.tag, distanceDiff : 100.0}) (DA.filter (\x -> (if placeId == "" then false else (fromMaybe "" x.placeId) == placeId)) (DA.filter (\x -> (toLower x.tag) /= (toLower excludeTag)) savedLocations))

compareByDistance :: DistInfo -> DistInfo -> Ordering
compareByDistance ( a) ( b) = compare (a.distanceDiff ) (b.distanceDiff)

getLocTag :: String -> Maybe LocItemType
getLocTag locTag = case locTag of
  "LOC_LIST"  -> Just LOC_LIST
  "CURR_LOC"  -> Just CURR_LOC
  "LOCATE_ON_MAP" -> Just LOCATE_ON_MAP
  _               -> Nothing

savedLocTransformer :: (Array SavedReqLocationAPIEntity) -> Array LocationListItemState
savedLocTransformer savedLocation =  (map (\ (SavedReqLocationAPIEntity item) ->
  {
  prefixImageUrl : fetchImage FF_ASSET $ case (toLower (item.tag) ) of 
                "home" -> "ny_ic_home_blue"
                "work" -> "ny_ic_work_blue"
                _      -> "ny_ic_fav_red"
, postfixImageUrl : ""
, postfixImageVisibility : false
, title : (fromMaybe "" (DA.head (split (Pattern ",") (decodeAddress(SavedLoc (SavedReqLocationAPIEntity item))))))
, subTitle : (drop ((fromMaybe 0 (indexOf (Pattern ",") (decodeAddress (SavedLoc (SavedReqLocationAPIEntity item))))) + 2) (decodeAddress (SavedLoc (SavedReqLocationAPIEntity item))))
, lat : (Just item.lat)
, lon : (Just item.lon)
, description : (fromMaybe "" (DA.head (split (Pattern ":") (decodeAddress (SavedLoc (SavedReqLocationAPIEntity item))))))
, placeId : item.placeId
, tag : item.tag
, tagType : Just (show LOC_LIST)
, cardType : Nothing
, address : ""
, tagName : ""
, isEditEnabled : true
, savedLocation : ""
, placeName : ""
, isClickable : true
, alpha : 1.0
, fullAddress : getAddressFromSaved (SavedReqLocationAPIEntity item)
, locationItemType : Just SAVED_LOCATION
, distance : Nothing
, showDistance : Just false
, actualDistance : Nothing
, frequencyCount : Nothing
, recencyDate : Nothing
, locationScore : Nothing
, dynamicAction : Nothing
, types : Nothing
}) savedLocation )