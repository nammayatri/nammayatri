{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.Handler where

import Prelude

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Error, makeAff, nonCanceler)
import Engineering.Helpers.BackTrack (getState)
import Helpers.Utils (getDistanceBwCordinates, LatLon(..), getCurrentLocation)
import JBridge (getCurrentPosition, getCurrentPositionWithTimeout)
import Log (printLog)
import Presto.Core.Types.Language.Flow (doAff)
import Presto.Core.Types.Language.Flow (getLogFields)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.HomeScreen.Controller (ScreenOutput(..))
import Screens.HomeScreen.View as HomeScreen
import Types.App (FlowBT, GlobalState(..), HOME_SCREENOUTPUT(..), ScreenType(..), NAVIGATION_ACTIONS(..))
import Types.ModifyScreenState (modifyScreenState)
import Screens.Types (KeyboardModalType(..))
import Data.Maybe (Maybe(..))
import Presto.Core.Types.Language.Flow (getLogFields)
import Helpers.Utils (getCurrentLocation, LatLon(..))


homeScreen :: FlowBT String HOME_SCREENOUTPUT
homeScreen = do
  logField_ <- lift $ lift $ getLogFields
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ HomeScreen.screen state.homeScreen{data{logField = logField_}}
  case action of
    GoToProfileScreen updatedState-> do
      modifyScreenState $ HomeScreenStateType (\homeScreen → updatedState)
      App.BackT $ App.BackPoint <$> pure GO_TO_PROFILE_SCREEN
    GoToHelpAndSupportScreen state -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → state)
      App.BackT $ App.BackPoint <$> pure GO_TO_HELP_AND_SUPPORT_SCREEN
    GotoEditGenderScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_EDIT_GENDER_SCREEN
    GoToRidesScreen updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen → updatedState)
      App.BackT $ App.BackPoint <$> pure GO_TO_RIDES_SCREEN
    GoToReferralScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_REFERRAL_SCREEN_FROM_HOME_SCREEN
    DriverAvailabilityStatus state status -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → state)
      App.BackT $ App.BackPoint <$> pure (DRIVER_AVAILABILITY_STATUS state status)
    StartRide state -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → state)
      LatLon lat lon <- getCurrentLocation state.data.currentDriverLat state.data.currentDriverLon  state.data.activeRide.src_lat state.data.activeRide.src_lon
      App.BackT $ App.NoBack <$> (pure $ GO_TO_START_RIDE {id: state.data.activeRide.id, otp : state.props.rideOtp , lat : lat , lon : lon } state) 
    StartZoneRide  state -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → state)
      LatLon lat lon <- getCurrentLocation state.data.currentDriverLat state.data.currentDriverLon  state.data.activeRide.src_lat state.data.activeRide.src_lon
      App.BackT $ App.NoBack <$> (pure $ GO_TO_START_ZONE_RIDE {otp : state.props.rideOtp , lat : lat , lon : lon }) 
    EndRide updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      LatLon lat lon <- getCurrentLocation updatedState.data.currentDriverLat updatedState.data.currentDriverLon  updatedState.data.activeRide.dest_lat updatedState.data.activeRide.dest_lon
      App.BackT $ App.NoBack <$> (pure $ GO_TO_END_RIDE {id : updatedState.data.activeRide.id, lat : lat, lon : lon})
    SelectListModal state -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen → state)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_CANCEL_RIDE {id : state.data.activeRide.id , info : state.data.cancelRideModal.selectedReasonDescription, reason : state.data.cancelRideModal.selectedReasonCode})
    Refresh state -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen → state)
      App.BackT $ App.BackPoint <$> pure REFRESH_HOME_SCREEN_FLOW
    UpdatedState screenState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen → screenState)
      App.BackT $ App.NoBack <$> (pure $ RELOAD screenState)
    UpdateRoute state -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreen → state)
      App.BackT $ App.BackPoint <$> pure (UPDATE_ROUTE state)
    FcmNotification notificationType screenState -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreen → screenState)
      App.BackT $ App.BackPoint <$> (pure $ FCM_NOTIFICATION notificationType)
    NotifyDriverArrived state -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreen → state)
      App.BackT $ App.BackPoint <$> (pure $ NOTIFY_CUSTOMER state)
    UpdateStage stage state -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen → state)
      App.BackT $ App.BackPoint <$> (pure $ UPDATE_STAGE stage)
    GoToNotifications updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen → updatedState)
      App.BackT $ App.BackPoint <$> pure GO_TO_NOTIFICATIONS
    AddAlternateNumber state -> do
      App.BackT $ App.BackPoint <$> (pure $ ADD_ALTERNATE_HOME)
    CallCustomer updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ ON_CALL updatedState)
    OpenPaymentPage updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ OPEN_PAYMENT_PAGE updatedState)
    AadhaarVerificationFlow updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_AADHAAR_VERIFICATION)
    SubscriptionScreen updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.NoBack <$> (pure $ HOMESCREEN_NAV GoToSubscription)
-- DTHS.GoToStart screenState -> do
--       (Location startRideCurrentLat startRideCurrentLiong) <- spy "george2" <$> (lift $ lift $ doAff $ makeAff \cb -> getCurrentPosition (cb <<< Right) Location $> nonCanceler)
--       _ <- pure $ spy "lat handler" startRideCurrentLat
--       _ <- pure $ spy "lon handler" startRideCurrentLong
--       App.BackT $ App.BackPoint <$> (pure $ ReachedPickUp screenState startRideCurrentLat startRideCurrentLong)