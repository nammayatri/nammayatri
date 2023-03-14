module Screens.AddNewAddressScreen.Controller where

import Accessor (_description, _place_id)
import Components.GenericHeader as GenericHeader
import Components.LocationListItem as LocationListItemController
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Array ((!!), length, filter, any, sortBy, null) as DA
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.String (trim, length, split, Pattern(..), drop, indexOf, toLower)
import Effect (Effect)
import Engineering.Helpers.Commons (os, getNewIDWithTag)
import Global (readFloat)
import Helpers.Utils (getCurrentLocationMarker, getDistanceBwCordinates, getLocationName)
import JBridge (animateCamera, currentPosition, exitLocateOnMap, hideKeyboardOnNavigation, isLocationEnabled, isLocationPermissionEnabled, locateOnMap, removeAllPolylines, requestKeyboardShow, requestLocation, toast, toggleBtnLoader)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, Ordering, Unit, bind, compare, discard, map, not, pure, unit, void, show, ($), (&&), (+), (-), (/=), (<>), (==), (>), (>=), (||))
import PrestoDOM (Eval, Visibility(..), continue, exit, updateAndExit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Resources.Constants (DecodeAddress(..), decodeAddress, getAddressFromSaved, getValueByComponent, getWard)
import Screens (ScreenName(..), getScreen)
import Screens.HomeScreen.ScreenData (dummyAddress)
import Screens.Types (AddNewAddressScreenState, CardType(..), Location, LocationListItemState, DistInfo, LocItemType(..), LocationItemType(..))
import Services.API (AddressComponents, Prediction, SavedReqLocationAPIEntity(..))
import Storage (KeyStore(..), getValueToLocalStore)

instance showAction :: Show Action where 
  show _ = ""

instance loggableAction :: Loggable Action where 
  performLog action appId = case action of 
    AfterRender -> trackAppScreenRender appId "screen" (getScreen ADD_NEW_ADDRESS_SCREEN)
    BackPressed backpressState -> do 
      trackAppBackPress appId (getScreen ADD_NEW_ADDRESS_SCREEN)
      case backpressState.props.isLocateOnMap , backpressState.props.editLocation , backpressState.props.showSavePlaceView , backpressState.props.fromHome of 
        true , _ , _ , _ -> trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "backpress_in_locate_on_map"
        _ , true , false , _ -> trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "backpress_in_edit_location"
        _ , _ , _ , true -> do 
          trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "backpress_in_from_home"
          trackAppEndScreen appId (getScreen ADD_NEW_ADDRESS_SCREEN)
        _ , _ , _ , _ -> do 
          trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "in_screen" "backpress_in_exit_locate_on_map"
          trackAppEndScreen appId (getScreen ADD_NEW_ADDRESS_SCREEN)
    GenericHeaderAC act -> case act of 
      GenericHeader.PrefixImgOnClick -> do 
        trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen ADD_NEW_ADDRESS_SCREEN)
      GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "generic_header_action" "forward_icon"
    LocationListItemAC act -> case act of 
      LocationListItemController.SelectedCurrentLocation lat lon name -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "location_list_item" "current_location"
      LocationListItemController.OnClick item -> trackAppActionClick appId (getScreen ADD_NEW_ADDRESS_SCREEN) "location_list_item" "location"
      LocationListItemController.FavClick act -> trackAppScreenEvent appId (getScreen ADD_NEW_ADDRESS_SCREEN) "location_list_item" "fav"
    PrimaryEditTextAC (PrimaryEditText.TextChanged id input) -> trackAppTextInput appId (getScreen ADD_NEW_ADDRESS_SCREEN)"save_as_text_changed" "primary_edit_text" 
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
            | SelectedCurrentLocation String String String
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
  _ <- pure $ locateOnMap true 0.0 0.0
  case key of 
    _ -> continueWithCmd state[ do
      _ <- checkPermissionAndUpdatePersonMarker state
      pure AfterRender
    ]

eval (ClearEditText) state = do 
  void $ pure $ requestKeyboardShow (getNewIDWithTag "SavedLocationEditText")
  continue state{props{isSearchedLocationServiceable = true}}

eval SetLocationOnMap state = do 
  _ <- pure $ locateOnMap true 0.0 0.0 
  _ <- pure $ currentPosition ""
  _ <- pure $ removeAllPolylines ""
  _ <- pure $ hideKeyboardOnNavigation true 
  _ <- pure $ toggleBtnLoader "" false
  let newState = state{props{isLocateOnMap = true}}
  continue newState

eval (UpdateLocation key lat lon) state = do
  case key of 
    "LatLon" -> do 
      exit $ UpdateLocationName state (fromMaybe 0.0 (fromString lat)) (fromMaybe 0.0 (fromString lon))
    _ -> continue state

eval (UpdateCurrLocName lat lon name) state = do 
  continue state {data{locSelectedFromMap = name, latSelectedFromMap = (fromMaybe 0.0 (fromString lat)), lonSelectedFromMap = (fromMaybe 0.0 (fromString lon))}}

eval (SelectedCurrentLocation lat lon name) state = do 
  void $ pure $ hideKeyboardOnNavigation true
  exit $ CheckLocServiceability (state{data{address = name, locationList = [] , selectedItem{tag = "Current_Location",lat = Just (fromMaybe 0.0 (fromString lat)), lon = Just (fromMaybe 0.0 (fromString lon)),placeId = Nothing, description = name,title =(fromMaybe "" ((split (Pattern "," ) name) DA.!! 0) ), subTitle = (drop ((fromMaybe 0 (indexOf (Pattern ",") (name))) + 2) (name))  }}}) (CURR_LOC)

eval (UpdateCurrentLocation lat lng) state = continue state{data{ lat = (fromMaybe 0.0 (fromString lat) ), lon = (fromMaybe 0.0 (fromString lng))}}

eval RecenterCurrentLocation state = continueWithCmd state [do 
  _ <- pure $ currentPosition ""
  pure NoAction
  ]

eval (BackPressed backpressState) state = do
  case state.props.isLocateOnMap , state.props.editLocation , state.props.showSavePlaceView , state.props.fromHome of 
    true , _ , _ , _ -> do 
      continue state{props{isLocateOnMap = false}, data{selectedItem{description = state.data.address},locationList = state.data.recentSearchs.predictionArray}} 
    _ , true , false , _ -> do 
        if state.data.activeIndex == Just 2 then pure unit 
          else void $ pure $ hideKeyboardOnNavigation true
        continue state {props{showSavePlaceView = true, isLocateOnMap = false}, data{address= state.data.selectedItem.description}}
    _ , _ , _ , true -> do 
      exit $ GoToHome 
    _ , _ , _ , _ -> do 
        void $ pure $ hideKeyboardOnNavigation true
        _ <- pure $ exitLocateOnMap ""
        updateAndExit state{props{editLocation = false}} $ GoToFavourites

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure $ BackPressed state]

eval (LocationListItemAC (LocationListItemController.SelectedCurrentLocation lat lon name)) state = do 
  let newState = state{data{ address = name ,selectedItem{lat = Just (fromMaybe 0.0 (fromString lat)), lon = Just (fromMaybe 0.0 (fromString lon)), description = name, placeId = Nothing }}}
  updateAndExit newState $  CheckLocServiceability (state{data{address= name , selectedItem{lat = Just (fromMaybe 0.0 (fromString lat)), lon = Just (fromMaybe 0.0 (fromString lon)), placeId = Nothing,description = name}}}) (CURR_LOC)

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
      void $ pure $ toast (getString LOCATION_ALREADY_EXISTS)
      continue state

eval (PrimaryButtonConfirmLocAC (PrimaryButton.OnClick)) state = do 
  _ <- pure $ exitLocateOnMap ""
  exit $ CheckLocServiceability (state{data { address = state.data.locSelectedFromMap
                                            , selectedItem{placeId = Nothing
                                            , description = state.data.locSelectedFromMap
                                            , title = (fromMaybe "" ((split (Pattern "," ) state.data.locSelectedFromMap)DA.!! 0) ) 
                                            , subTitle = (drop ((fromMaybe 0 (indexOf (Pattern ",") (state.data.locSelectedFromMap))) + 2) (state.data.locSelectedFromMap))
                                            , lat = Just state.data.latSelectedFromMap
                                            , lon = Just state.data.lonSelectedFromMap }
                                            }
                                          }) (LOCATE_ON_MAP)
  
eval (TagSelected index) state = do
  if (index == 2) then void $ pure $ requestKeyboardShow (getNewIDWithTag "SaveAsEditText")
    else void $ pure $ hideKeyboardOnNavigation true
  let activeTag = case index of 
                    0 -> "Home"
                    1 -> "Work"
                    _ -> state.data.addressSavedAs
  if (activeTag == "") then  continue state{props{isBtnActive = false}, data {activeIndex = Just index,  selectedTag = getTag index }}
    else
      if (validTag state.data.savedTags activeTag state.data.placeName) then 
        continue state{ data  { activeIndex = Just index
                              , selectedTag = getTag index}
                      , props { isBtnActive = if (index == 2 && state.data.addressSavedAs /= "") || (index == 2 && state.props.editLocation == true && state.data.placeName /="" )  
                                                then (not state.props.placeNameExists && true) 
                                                else if (index == 1 || index == 0) 
                                                  then (not state.props.placeNameExists && true) 
                                                    else false
                                                    }} 
        else do 
          void $ pure $ toast ((case (toLower activeTag) of 
                                  "home" -> (getString HOME) 
                                  "work" -> (getString WORK) 
                                  _      -> "") <> " " <> (getString LOCATION_ALREADY_EXISTS))
          continue state

eval (ChangeAddress ) state = do 
  continue state{props{showSavePlaceView = false, editLocation = true, isSearchedLocationServiceable = state.props.isLocationServiceable},data{latSelectedFromMap = state.data.lat , lonSelectedFromMap = state.data.lon ,locationList= state.data.recentSearchs.predictionArray}}

eval (PrimaryEditTextAC (PrimaryEditText.TextChanged id input)) state = do 
  case (trim input) of 
    "" ->  continue state{props{isBtnActive = false}, data{addressSavedAs = ""}}
    _  -> if (validTag state.data.savedTags (trim input) state.data.placeName) then 
            if (length (trim input) >= 3 ) then continue state {data{addressSavedAs =(trim input)},props{isBtnActive = if (state.data.selectedTag /= Nothing) then true else false, placeNameExists = false}}
              else continue state {data{addressSavedAs = ""}, props{isBtnActive = if state.data.selectedTag /= Just OTHER_TAG then true else false, placeNameExists = false}}
            else continue state{props{isBtnActive = false, placeNameExists = true}}

eval (PrimaryButtonAC (PrimaryButton.OnClick)) state = do 
  void $ pure $ hideKeyboardOnNavigation true
  updateAndExit state $ AddLocation state
eval _ state = continue state

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
  , prefixImageUrl : "ny_ic_loc_grey,https://assets.juspay.in/nammayatri/images/user/ny_ic_loc_grey.png"
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
                        Just $ getValueByComponent addressComponents "sublocality"
                }

getSavedLocations :: (Array SavedReqLocationAPIEntity) -> Array LocationListItemState 
getSavedLocations savedLocation =  (map (\ (SavedReqLocationAPIEntity item) -> 
  {
  prefixImageUrl : case (toLower (item.tag) ) of 
                "home" -> "ny_ic_home_blue,https://assets.juspay.in/nammayatri/images/user/ny_ic_home_blue.png"
                "work" -> "ny_ic_work_blue,https://assets.juspay.in/nammayatri/images/user/ny_ic_work_blue.png"
                _      -> "ny_ic_fav_red,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav_red.png"
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
}) savedLocation )

getSavedTags :: (Array SavedReqLocationAPIEntity) -> Array String 
getSavedTags savedLocation = (map (\(SavedReqLocationAPIEntity item) -> toLower (item.tag) ) savedLocation)

getSavedTagsFromHome :: (Array LocationListItemState) -> Array String 
getSavedTagsFromHome savedLocation = (map (\(item) -> toLower (item.tag)) savedLocation)

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
    _ <- getLocationName (showPersonMarker state (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))) "9.9" "9.9" "Current Location" constructLatLong
    pure unit
    else do 
      if (os == "IOS" && conditionC) then do 
        _ <- getLocationName (showPersonMarker state (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))) "9.9" "9.9" "Current Location" constructLatLong
        pure unit 
        else do 
          _ <- requestLocation unit
          _ <- checkPermissionAndUpdatePersonMarker state
          pure unit

showPersonMarker :: AddNewAddressScreenState -> String -> Location -> Effect Unit
showPersonMarker state marker location = animateCamera location.lat location.lng 19

constructLatLong :: String -> String -> String -> Location
constructLatLong lat lng _ =
  { lat: readFloat lat
  , lng : readFloat lng
  , place : ""
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
