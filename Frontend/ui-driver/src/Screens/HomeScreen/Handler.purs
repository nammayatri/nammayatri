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
import Data.Maybe (Maybe(..))
import Effect.Aff (Error, makeAff, nonCanceler)
import Engineering.Helpers.BackTrack (getState)
import Helpers.Utils (getCurrentLocation, LatLon(..))
import Helpers.Utils (getDistanceBwCordinates, LatLon(..), getCurrentLocation)
import JBridge (getCurrentPosition, getCurrentPositionWithTimeout)
import Log (printLog)
import Presto.Core.Types.Language.Flow (doAff)
import Presto.Core.Types.Language.Flow (getLogFields)
import Presto.Core.Types.Language.Flow (getLogFields)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.HomeScreen.Controller (ScreenOutput(..))
import Screens.HomeScreen.View as HomeScreen
import Screens.Types (KeyboardModalType(..))
import Types.App (FlowBT, GlobalState(..), HOME_SCREENOUTPUT(..), ScreenType(..), NAVIGATION_ACTIONS(..))
import Types.ModifyScreenState (modifyScreenState)


homeScreen :: FlowBT String HOME_SCREENOUTPUT
homeScreen = do
  logField_ <- lift $ lift $ getLogFields
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ HomeScreen.screen state.homeScreen{data{logField = logField_}}
  case action of
    GoToVehicleDetailScreen updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure GO_TO_VEHICLE_DETAILS_SCREEN
    GoToProfileScreen updatedState-> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure GO_TO_PROFILE_SCREEN
    GoToHelpAndSupportScreen updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure GO_TO_HELP_AND_SUPPORT_SCREEN
    GotoEditGenderScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_EDIT_GENDER_SCREEN
    GoToRidesScreen updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure GO_TO_RIDES_SCREEN
    GoToReferralScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_REFERRAL_SCREEN_FROM_HOME_SCREEN
    DriverAvailabilityStatus updatedState status -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure (DRIVER_AVAILABILITY_STATUS updatedState status)
    StartRide updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      LatLon lat lon <- getCurrentLocation updatedState.data.currentDriverLat updatedState.data.currentDriverLon  updatedState.data.activeRide.src_lat updatedState.data.activeRide.src_lon 700
      App.BackT $ App.NoBack <$> (pure $ GO_TO_START_RIDE {id: updatedState.data.activeRide.id, otp : updatedState.props.rideOtp , lat : lat , lon : lon } updatedState) 
    StartZoneRide  updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      LatLon lat lon <- getCurrentLocation updatedState.data.currentDriverLat updatedState.data.currentDriverLon  updatedState.data.activeRide.src_lat updatedState.data.activeRide.src_lon 1000
      App.BackT $ App.NoBack <$> (pure $ GO_TO_START_ZONE_RIDE {otp : updatedState.props.rideOtp , lat : lat , lon : lon }) 
    EndRide updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      LatLon lat lon <- getCurrentLocation updatedState.data.currentDriverLat updatedState.data.currentDriverLon  updatedState.data.activeRide.dest_lat updatedState.data.activeRide.dest_lon 700
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_END_RIDE {id : updatedState.data.activeRide.id, lat : lat, lon : lon})
    SelectListModal updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_CANCEL_RIDE {id : updatedState.data.activeRide.id , info : updatedState.data.cancelRideModal.selectedReasonDescription, reason : updatedState.data.cancelRideModal.selectedReasonCode})
    Refresh updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure REFRESH_HOME_SCREEN_FLOW
    UpdatedState screenState -> do
      modifyScreenState $ HomeScreenStateType (\_ → screenState)
      App.BackT $ App.NoBack <$> (pure $ RELOAD screenState)
    UpdateRoute updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure (UPDATE_ROUTE updatedState)
    FcmNotification notificationType screenState -> do 
      modifyScreenState $ HomeScreenStateType (\_ → screenState)
      App.BackT $ App.BackPoint <$> (pure $ FCM_NOTIFICATION notificationType)
    NotifyDriverArrived updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ NOTIFY_CUSTOMER updatedState)
    UpdateStage stage updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ UPDATE_STAGE stage)
    GoToNotifications updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure GO_TO_NOTIFICATIONS
    AddAlternateNumber updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ ADD_ALTERNATE_HOME)
    CallCustomer updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ ON_CALL updatedState)
    OpenPaymentPage updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ OPEN_PAYMENT_PAGE updatedState)
    AadhaarVerificationFlow updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_AADHAAR_VERIFICATION)
    SubscriptionScreen updatedState goToBottom -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $  App.NoBack <$> (pure $ HOMESCREEN_NAV GoToSubscription goToBottom)
    GoToRideDetailsScreen updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_RIDE_DETAILS_SCREEN)
    PostRideFeedback updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ POST_RIDE_FEEDBACK updatedState)
-- DTHS.GoToStart screenState -> do
--       (Location startRideCurrentLat startRideCurrentLiong) <- spy "george2" <$> (lift $ lift $ doAff $ makeAff \cb -> getCurrentPosition (cb <<< Right) Location $> nonCanceler)
--       _ <- pure $ spy "lat handler" startRideCurrentLat
--       _ <- pure $ spy "lon handler" startRideCurrentLong
--       App.BackT $ App.BackPoint <$> (pure $ ReachedPickUp screenState startRideCurrentLat startRideCurrentLong)