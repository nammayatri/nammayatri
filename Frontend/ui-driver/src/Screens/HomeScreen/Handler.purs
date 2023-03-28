{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.Handler where

import Log (printLog)
import Prelude
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Data.Either (Either(..))
import Effect.Aff (nonCanceler, makeAff)
import Engineering.Helpers.BackTrack (getState)
import JBridge (getCurrentPosition)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.HomeScreen.Controller (ScreenOutput(..))
import Screens.HomeScreen.View as HomeScreen
import Types.App (FlowBT, GlobalState(..), HOME_SCREENOUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)

data Location = Location String String

homeScreen :: FlowBT String HOME_SCREENOUTPUT
homeScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ HomeScreen.screen state.homeScreen
  case action of
    GoToProfileScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_PROFILE_SCREEN
    GoToRidesScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_RIDES_SCREEN
    GoToReferralScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_REFERRAL_SCREEN_FROM_HOME_SCREEN
    DriverAvailabilityStatus state status -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → state)
      App.BackT $ App.BackPoint <$> pure (DRIVER_AVAILABILITY_STATUS status)
    StartRide  state -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → state)
      (Location startRideCurrentLat startRideCurrentLong) <- (lift $ lift $ doAff $ makeAff \cb -> getCurrentPosition (cb <<< Right) Location $> nonCanceler)
      _ <- pure $ printLog "lat handler" startRideCurrentLat
      _ <- pure $ printLog "lon handler" startRideCurrentLong
      App.BackT $ App.NoBack <$> (pure $ GO_TO_START_RIDE {id: state.data.activeRide.id, otp : state.props.rideOtp , lat : startRideCurrentLat, lon : startRideCurrentLong})
    StartZoneRide  state -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → state)
      (Location startZoneRideCurrentLat startZoneRideCurrentLong) <- (lift $ lift $ doAff $ makeAff \cb -> getCurrentPosition (cb <<< Right) Location $> nonCanceler)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_START_ZONE_RIDE {otp : state.props.rideOtp , lat : startZoneRideCurrentLat, lon : startZoneRideCurrentLong})
    EndRide updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      (Location endRideCurrentLat endRideCurrentLong) <- (lift $ lift $ doAff $ makeAff \cb -> getCurrentPosition (cb <<< Right) Location $> nonCanceler)
      _ <- pure $ printLog "lat handler" endRideCurrentLat
      _ <- pure $ printLog "lon handler" endRideCurrentLong
      modifyScreenState $ HomeScreenStateType (\homeScreen → updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_END_RIDE {id : updatedState.data.activeRide.id, lat : endRideCurrentLat, lon : endRideCurrentLong})
    CancelRide state -> do
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
    GoToNotifications -> App.BackT $ App.BackPoint <$> pure GO_TO_NOTIFICATIONS
-- DTHS.GoToStart screenState -> do
--       (Location startRideCurrentLat startRideCurrentLiong) <- spy "george2" <$> (lift $ lift $ doAff $ makeAff \cb -> getCurrentPosition (cb <<< Right) Location $> nonCanceler)
--       _ <- pure $ spy "lat handler" startRideCurrentLat
--       _ <- pure $ spy "lon handler" startRideCurrentLong
--       App.BackT $ App.BackPoint <$> (pure $ ReachedPickUp screenState startRideCurrentLat startRideCurrentLong)