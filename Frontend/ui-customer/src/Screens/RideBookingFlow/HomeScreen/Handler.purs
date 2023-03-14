module Screens.HomeScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import JBridge (toggleLoader)
import ModifyScreenState (modifyScreenState)
import Prelude (bind, discard, ($), (<$>), pure, void)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.HomeScreen.Controller (ScreenOutput(..))
import Screens.HomeScreen.View as HomeScreen
import Types.App (FlowBT, GlobalState(..), ScreenType(..), HOME_SCREEN_OUTPUT(..))

homeScreen ::FlowBT String HOME_SCREEN_OUTPUT
homeScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ HomeScreen.screen state.homeScreen
  void $ lift $ lift $ toggleLoader false
  case act of
    UpdateLocationName updatedState lat lng-> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> pure (UPDATE_LOCATION_NAME updatedState lat lng)
    UpdatePickupName updatedState lat lng -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> pure (UPDATE_PICKUP_NAME updatedState lat lng)
    PastRides updatedState-> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure GO_TO_MY_RIDES)
    GoToHelp updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure GO_TO_HELP)
    ChangeLanguage updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure CHANGE_LANGUAGE)
    GoToAbout updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure GO_TO_ABOUT)
    GoToMyProfile updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure GO_TO_MY_PROFILE)
    GoToFavourites updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure GO_TO_FAVOURITES_ )
    LocationSelected selectedItem addToRecents updatedState -> do 
          modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
          App.BackT $ App.BackPoint <$> (pure $ LOCATION_SELECTED selectedItem addToRecents)
    SearchPlace input updatedState -> do
          modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
          App.BackT $ App.NoBack <$> (pure $ SEARCH_LOCATION input updatedState)
    GetQuotes updatedState -> do 
          modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
          App.BackT $ App.NoBack <$> (pure $ GET_QUOTES updatedState)  
    LogoutUser -> App.BackT $ App.NoBack <$> (pure $ LOGOUT)
    SelectEstimate updatedState -> do
          modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
          App.BackT $ App.NoBack <$> (pure $ SELECT_ESTIMATE updatedState)  
    GetSelectList updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GET_SELECT_LIST updatedState)  
    ConfirmRide updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ CONFIRM_RIDE updatedState) 
    RideConfirmed updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ ONGOING_RIDE updatedState) 
    CancelRide updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ CANCEL_RIDE_REQUEST updatedState)   
    UpdatedState screenState saveToCurrLocs -> do 
       modifyScreenState $ HomeScreenStateType (\homeScreenState → screenState)
       App.BackT $ App.BackPoint <$> (pure $ RELOAD saveToCurrLocs)
    NotificationHandler notification updatedState ->  do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ FCM_NOTIFICATION notification updatedState)
    Cancel updatedState -> do 
        modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
        App.BackT $ App.NoBack <$> (pure $ CANCEL)
    Retry updatedState -> do 
        modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
        App.BackT $ App.NoBack <$> (pure $ RETRY)
    GoToHome -> App.BackT $ App.NoBack <$> (pure $ HOME_SCREEN)
    SubmitRating updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ SUBMIT_RATING updatedState)
    OpenGoogleMaps updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ OPEN_GOOGLE_MAPS updatedState)
    InAppTrackStatus updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ IN_APP_TRACK_STATUS updatedState)
    UpdatedSource updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_FIND_ESTIMATES updatedState)
    UpdateSavedLocation screenState -> do 
       modifyScreenState $ HomeScreenStateType (\homeScreenState → screenState)
       App.BackT $ App.BackPoint <$> (pure $ UPDATE_SAVED_LOCATION )
    CheckLocServiceability updatedState lat long -> do 
      App.BackT $ App.NoBack <$> (pure $ CHECK_SERVICEABILITY updatedState lat long)
    GoToInvoice updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.BackPoint <$> pure (GO_TO_INVOICE_ updatedState)
    CheckFavDistance updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ CHECK_FOR_DUPLICATE_SAVED_LOCATION updatedState) 
    SaveFavourite updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ SAVE_FAVOURITE updatedState)
    GoToReferral updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_REFERRAL)
      
