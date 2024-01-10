module Screens.SearchLocationScreen.Controller where

import Prelude
import PrestoDOM (Eval, continue, exit, continueWithCmd, updateAndExit)
import Screens.Types (SearchLocationScreenState, SearchLocationTextField(..), SearchLocationStage(..), LocationListItemState)
import Components.LocationTagBarV2 as LocationTagBarController
import Components.LocationListItem as LocationListItemController
import Components.FavouriteLocationModel as FavouriteLocModelController
import Components.GenericHeader as GenericHeaderController
import Components.SaveFavouriteCard as SaveFavCardController 
import Components.PrimaryEditText as EditTextController
import PrestoDOM.Types.Core (class Loggable)
import Log (trackAppActionClick)
import Screens (getScreen, ScreenName(..))
import Data.String(length, trim, toLower) as STR
import Data.Array (length, find, sortBy) as DA
import Debug (spy)
import JBridge (currentPosition, toast, hideKeyboardOnNavigation, updateInputString)
import Data.Maybe (fromMaybe, Maybe(..), isJust, maybe ) as MB
import Mobility.Prelude (strToNumber)
import Helpers.Utils (recentDistance)
import Data.Ord (comparing)

instance showAction :: Show Action where 
  show _ = ""

instance loggableAction :: Loggable Action where 
  performLog action appId = case action of 
    _ -> trackAppActionClick appId (getScreen SEARCH_LOCATION_SCREEN) "primary_button_action" "no_action"


data Action = NoAction 
            | MapReady String String String
            | AfterRender
            | InputChanged String
            | TextFieldFocusChanged SearchLocationTextField
            | UpdateLocAndLatLong (Array LocationListItemState) String String 
            | RecenterCurrentLocation 
            | SetLocationOnMap 
            | AutoCompleteCallBack String Boolean
            | LocationTagBarAC (Array LocationListItemState) LocationTagBarController.Action
            | LocationListItemAC (Array LocationListItemState) LocationListItemController.Action 
            | FavouriteLocationModelAC FavouriteLocModelController.Action
            | SaveFavCardAC (Array LocationListItemState) SaveFavCardController.Action 

data ScreenOutput = NoOutput  
                  | SearchPlace String SearchLocationScreenState
                  | SaveFavLoc SearchLocationScreenState (Array LocationListItemState)
                  | ConfirmAndSaveFav SearchLocationScreenState
                  | AddFavLoc SearchLocationScreenState String
                  | PredictionClicked LocationListItemState SearchLocationScreenState

eval :: Action -> SearchLocationScreenState -> Eval Action ScreenOutput SearchLocationScreenState

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
      let updatedLoc = {placeId : item.placeId, address : item.description, lat : item.lat, lon : item.lon}
      if currTextField == SearchLocPickup then 
        exit $ PredictionClicked item state { data { srcLoc = MB.Just updatedLoc }} 
        else
          exit $ PredictionClicked item state { data { destLoc = MB.Just updatedLoc}}


eval (FavouriteLocationModelAC (FavouriteLocModelController.GenericHeaderAC (GenericHeaderController.PrefixImgOnClick))) state = continue state{props{searchLocStage = PredictionsStage}}

eval (LocationTagBarAC savedLoc (LocationTagBarController.TagClicked tag) ) state = do 
  case tag of 
    "ADD_HOME" ->  if DA.length savedLoc >= 20 then 
      continue state 
      else exit $ AddFavLoc state "HOME_TAG"
    "ADD_WORK" -> if DA.length savedLoc >= 20 then 
      continue state 
      else exit $ AddFavLoc state "WORK_TAG"
    "HOME" -> continue state 
    "WORK" -> continue state 
    _ -> continue state{ props {searchLocStage = AllFavouritesStage}}

eval (TextFieldFocusChanged textField) state = continue state{props{focussedTextField = MB.Just textField}}

eval (AutoCompleteCallBack value pickUpchanged) state =   
  autoCompleteAPI state value $ if pickUpchanged then SearchLocPickup else SearchLocDrop

eval (InputChanged value) state = do 
  continueWithCmd state {props { canClearText = STR.length value > 2}} [ do 
    void $ pure $ updateInputString value 
    pure NoAction
  ]
  
eval (UpdateLocAndLatLong recentSearches lat lng) state = do 
  let updatedLoc = {placeId : MB.Nothing, address : "Current Location" , lat : strToNumber lat , lon : strToNumber lng}
  continue state{ data 
                    { srcLoc = MB.Just updatedLoc
                    , currentLoc = MB.Just updatedLoc
                    , locationList = DA.sortBy (comparing (_.actualDistance)) $ recentDistance recentSearches (MB.fromMaybe 0.0 updatedLoc.lat) (MB.fromMaybe 0.0 updatedLoc.lon) }
                    }

eval RecenterCurrentLocation state = continueWithCmd state [ do 
  _ <- pure $ currentPosition ""
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

eval (SetLocationOnMap) state = continue state

eval _ state = continue state


autoCompleteAPI state inputStr inputType = do 
  if (STR.length $ STR.trim inputStr) > 2 && inputStr /= "Current Location" then do 
    callSearchLocationAPI 
    else continue state{props{canClearText = false}}
  where 
    callSearchLocationAPI = do 
      let newState = state{props{ canSelectFromFav = false, showLoader = true, canClearText = true}}
      updateAndExit newState $ SearchPlace inputStr newState
