{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTIEHULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SavedLocationScreen.Controller where


import Prelude( class Show, pure, unit, bind, map, discard, show, not, ($),(==), (&&), (+), (/=), (<>), (||), (>=))
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import PrestoDOM (Eval, Visibility(..), continue, exit, continueWithCmd, updateAndExit)
import Screens.Types(SavedLocationScreenState, SavedLocationData, LocationListItemState, LocationItemType(..))
import Components.GenericHeader.Controller as GenericHeaderController
import Components.SavedLocationCard as SavedLocationCardController
import Components.PrimaryButton as PrimaryButtonController
import Components.ErrorModal as ErrorModalController
import Components.PopUpModal as PopUpModal
import Services.API(SavedReqLocationAPIEntity(..), SavedLocationsListRes)
import Data.String (trim, toLower, split, Pattern(..))
import Data.Array (filter, (!!), length)
import Data.Maybe (fromMaybe, Maybe(..))
import Resources.Constants (DecodeAddress(..), decodeAddress, getAddressFromSaved)
import JBridge (toast, toggleBtnLoader)
import Language.Strings(getString)
import Language.Types(STR(..))
import Accessor (_list)
import Data.Lens ((^.))
import Log (trackAppActionClick, trackAppEndScreen, trackAppBackPress, trackAppScreenRender, trackAppScreenEvent, trackAppTextInput)
import Screens (ScreenName(..), getScreen)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Engineering.Helpers.Utils (toggleLoader)
import Engineering.Helpers.MobilityPrelude
import Common.Types.App (LazyCheck(..))
import Engineering.Helpers.LogEvent (logEvent)
import Foreign.Object (empty)
import Effect.Unsafe (unsafePerformEffect)

instance showAction :: Show Action where 
  show _ = ""

instance loggableAction :: Loggable Action where 
  performLog action appId = case action of 
    AfterRender -> trackAppScreenRender appId "screen" (getScreen SAVED_LOCATION_SCREEN)
    BackPressed flag -> do 
      trackAppBackPress appId (getScreen SAVED_LOCATION_SCREEN)
      if flag then trackAppScreenEvent appId (getScreen SAVED_LOCATION_SCREEN) "in_screen" "backpress_in_request_delete_fav_location"
        else trackAppEndScreen appId (getScreen SAVED_LOCATION_SCREEN)
    GenericHeaderAC act -> case act of 
      GenericHeaderController.PrefixImgOnClick -> do 
        trackAppActionClick appId (getScreen SAVED_LOCATION_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen SAVED_LOCATION_SCREEN)
      GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen SAVED_LOCATION_SCREEN) "generic_header_action" "forward_icon"
    SavedLocationCardAction act -> case act of 
      SavedLocationCardController.DeleteLocation tagName -> trackAppActionClick appId (getScreen SAVED_LOCATION_SCREEN) "saved_location_card" "delete_location"
      SavedLocationCardController.EditLocation cardState -> trackAppActionClick appId (getScreen SAVED_LOCATION_SCREEN) "saved_location_card" "edit_location"
      SavedLocationCardController.CardClicked act -> trackAppActionClick appId (getScreen SAVED_LOCATION_SCREEN) "saved_location_card" "card_clicked"
      SavedLocationCardController.NoAction -> trackAppActionClick appId (getScreen SAVED_LOCATION_SCREEN) "saved_location_card" "no_action"
    PrimaryButtonAC act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen SAVED_LOCATION_SCREEN) "primary_button" "add_new_favorite"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen SAVED_LOCATION_SCREEN) "primary_button" "no_action"
    ErrorModalAC act -> case act of 
      ErrorModalController.PrimaryButtonActionController act -> case act of
        PrimaryButtonController.OnClick -> do
          trackAppActionClick appId (getScreen SAVED_LOCATION_SCREEN) "error_modal_action" "primary_button"
          trackAppEndScreen appId (getScreen SAVED_LOCATION_SCREEN)
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen SAVED_LOCATION_SCREEN) "error_modal_action" "primary_button_no_action"
    PopUpModalAction act -> case act of 
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen SAVED_LOCATION_SCREEN) "popup_modal_action" "on_goback"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen SAVED_LOCATION_SCREEN) "popup_modal_action" "on_delete_location"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen SAVED_LOCATION_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen SAVED_LOCATION_SCREEN) "popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen SAVED_LOCATION_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen SAVED_LOCATION_SCREEN) "popup_modal_action" "countdown_updated"
      PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen SAVED_LOCATION_SCREEN) "popup_modal_action" "tip_clicked"
      PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen SAVED_LOCATION_SCREEN) "popup_modal_action" "secondary_text_clicked"
      PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen SAVED_LOCATION_SCREEN) "popup_modal_action" "popup_dismissed"
      PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen SAVED_LOCATION_SCREEN) "popup_modal_action" "option_with_html_clicked"
    SavedLocationListAPIResponseAction respList -> trackAppScreenEvent appId (getScreen SAVED_LOCATION_SCREEN) "in_screen" "saved_location_list"
    NoAction -> trackAppScreenEvent appId (getScreen SAVED_LOCATION_SCREEN) "in_screen" "no_action"

    

data ScreenOutput = EditLocation LocationListItemState
                  | DeleteLocation String
                  | AddLocation SavedLocationScreenState
                  | GoBack

data Action = BackPressed Boolean
            | NoAction
            | GenericHeaderAC GenericHeaderController.Action
            | SavedLocationCardAction SavedLocationCardController.Action
            | SavedLocationListAPIResponseAction (SavedLocationsListRes)
            | PrimaryButtonAC PrimaryButtonController.Action
            | ErrorModalAC ErrorModalController.Action
            | PopUpModalAction PopUpModal.Action
            | AfterRender

eval :: Action -> SavedLocationScreenState -> Eval Action ScreenOutput SavedLocationScreenState

eval (SavedLocationCardAction (SavedLocationCardController.EditLocation cardState)) state = exit $ EditLocation cardState

eval (BackPressed flag) state = do 
  if state.props.showDeleteLocationModel then continue state{props{showDeleteLocationModel = false}, data{deleteTag = Nothing}}
    else exit $ GoBack

eval (SavedLocationCardAction (SavedLocationCardController.DeleteLocation tagName)) state = do 
  continue state{props{showDeleteLocationModel = true}, data{deleteTag = (Just tagName)}}

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state{props{showDeleteLocationModel = false}, data{deleteTag = Nothing}}
eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = exit $ DeleteLocation (fromMaybe "" state.data.deleteTag)
eval (GenericHeaderAC (GenericHeaderController.PrefixImgOnClick)) state = exit $ GoBack

eval (SavedLocationListAPIResponseAction respList) state = do 
  _ <- pure $ toggleLoader false
  let home = (filter (\x -> (toLower x.tag) == "home") (getSavedLocation (respList ^. _list)))
  let work = (filter (\x -> (toLower x.tag) == "work") (getSavedLocation (respList ^. _list)))
  let otherLocation = (filter (\x -> not  ((toLower x.tag) == "home" || (toLower x.tag) == "work")) (getSavedLocation (respList ^. _list)))
  continue state{data{savedLocations = home <> work <> otherLocation}, props{apiRespReceived = true}}

eval (PrimaryButtonAC (PrimaryButtonController.OnClick)) state = do 
  if (length state.data.savedLocations >= 20) then do 
    _ <- pure $ toast (getString SORRY_LIMIT_EXCEEDED_YOU_CANT_ADD_ANY_MORE_FAVOURITES)
    _ <- pure $ toggleBtnLoader "" false
    continue state
    else do
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_add_favourite_click"
      updateAndExit state $ AddLocation state 

eval (ErrorModalAC (ErrorModalController.PrimaryButtonActionController PrimaryButtonController.OnClick))state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_add_favourite_click_error_model"
  updateAndExit state $ AddLocation state

eval _ state = continue state



getSavedLocation :: (Array SavedReqLocationAPIEntity) -> Array LocationListItemState 
getSavedLocation (savedLocation) = (map (\(SavedReqLocationAPIEntity item) -> 
  { placeName : decodePlace (SavedReqLocationAPIEntity item)
  , address : decodeAddress (SavedLoc (SavedReqLocationAPIEntity item))
  , tag : item.tag
  , lat : Just item.lat
  , lon : Just item.lon
  , placeId : item.placeId
  , prefixImageUrl : ""
  , postfixImageUrl : ""
  , postfixImageVisibility : false 
  , subTitle : ""
  , title : ""
  , description : ""
  , tagType : Nothing 
  , cardType : Nothing 
  , tagName : ""
  , isEditEnabled : true 
  , savedLocation : ""
  , isClickable : true 
  , alpha : 1.0
  , fullAddress : getAddressFromSaved (SavedReqLocationAPIEntity item)
  , locationItemType : Just SAVED_LOCATION
  , distance : Nothing
  , showDistance : Just false
  , actualDistance : Nothing
  }
  )savedLocation)

getSavedLocationForAddNewAddressScreen :: (Array LocationListItemState) -> Array LocationListItemState 
getSavedLocationForAddNewAddressScreen (savedLocation) = (map (\ (item) -> 
  { prefixImageUrl : fetchImage FF_ASSET "ny_ic_loc_grey"
  , postfixImageUrl : ""
  , postfixImageVisibility : false
  , title : (fromMaybe "" ((split (Pattern ",") (item.address)) !! 0))
  , subTitle : ""
  , placeId : item.placeId 
  , lat : item.lat
  , lon : item.lon
  , description : item.address
  , tag : item.tag
  , tagType : Nothing
  , cardType : item.cardType
  , address : item.address
  , tagName : item.tagName 
  , isEditEnabled : true
  , savedLocation : ""
  , placeName : ""
  , isClickable : true 
  , alpha : 1.0
  , fullAddress : item.fullAddress
  , locationItemType :Just SAVED_LOCATION
  , distance : Nothing
  , showDistance : Just false
  , actualDistance : Nothing
  }
  ) (savedLocation) ) 

decodePlace :: SavedReqLocationAPIEntity -> String 
decodePlace (SavedReqLocationAPIEntity address )= 
  if ((isStrEmpty $ trim (fromMaybe "" address.area)) &&(isStrEmpty $ trim (fromMaybe "" address.street)) &&(isStrEmpty $ trim (fromMaybe "" address.door)) &&(isStrEmpty $ trim (fromMaybe "" address.building)) ) then
                (fromMaybe "" address.city) 
        else if ((isStrEmpty $ trim (fromMaybe "" address.street)) && (isStrEmpty $ trim (fromMaybe "" address.door)) &&(isStrEmpty $ trim (fromMaybe "" address.building)) ) then
                (fromMaybe "" address.area)
        else if ((isStrEmpty $ trim (fromMaybe "" address.door)) && (isStrEmpty $ trim (fromMaybe "" address.building))) then
                (fromMaybe "" address.street) 
        else if ((isStrEmpty $trim (fromMaybe "" address.door))) then
                (fromMaybe "" address.building) 
        else
                (fromMaybe "" address.door) 


