{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideCompletedScreen.Controller where

import Prelude (class Eq, (==), class Show, pure, unit, bind, ($), discard, not)
import Screens.Types as ST
import PrestoDOM.Types.Core (class Loggable)
import PrestoDOM (Eval, update, exit, continue)
import Log (trackAppActionClick, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Components.PrimaryButton as PrimaryButton
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Components.PopUpModal.Controller as PopUpModal
import Effect.Unsafe (unsafePerformEffect)
import Helpers.Utils as HU

data RatingType = POSITIVE | NEGATIVE
derive instance genericRatingType :: Generic RatingType _
instance showRatingType :: Show RatingType where show = genericShow
instance eqRatingType :: Eq RatingType where eq = genericEq


instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    GoToHomeScreenAction -> trackAppActionClick appId (getScreen RIDE_COMPLETED_SCREEN) "in_screen" "go_to_home_screen_from_ride_completed_screen"
    NoAction -> trackAppScreenEvent appId (getScreen TRIP_DETAILS_SCREEN) "in_screen" "no_action"
    _ -> pure unit -- change this

data Action = GoToHomeScreenAction
            | RideCompletedButtonClick PrimaryButton.Action
            | ViewFareBreakDown
            | NoAction
            | SelectRating RatingType
            | ShowRideDetails
            | HelpAndSupportAC
            | GoToCallSupportAction PopUpModal.Action 

data ScreenOutput = GoToHomeScreen ST.RideCompletedScreenState | GoToRideDetailsScreen ST.RideCompletedScreenState

eval :: Action -> ST.RideCompletedScreenState -> Eval Action ScreenOutput ST.RideCompletedScreenState

eval GoToHomeScreenAction state = exit $ GoToHomeScreen state

eval (RideCompletedButtonClick PrimaryButton.OnClick) state = exit $ GoToHomeScreen state

eval ShowRideDetails state = exit $ GoToRideDetailsScreen state

eval HelpAndSupportAC state = continue state {props{showCallSupportPopup = true}}

eval (ViewFareBreakDown) state = continue state { props {isFareBreakDownVisible = not state.props.isFareBreakDownVisible}}

eval (SelectRating rating) state = continue state {props {selectedRating = if rating == POSITIVE then ST.SEL_P else ST.SEL_N}} 

eval (GoToCallSupportAction PopUpModal.OnButton1Click) state = continue state {props {showCallSupportPopup  = false}}

eval (GoToCallSupportAction PopUpModal.OnButton2Click) state =  do
                                                                pure $ unsafePerformEffect $ HU.contactSupportNumber "" -- unsafePerformEffect is temporary fix. Need to update this.
                                                                continue state {props {showCallSupportPopup  = false}} -- @Vicky - what needs to be done here
eval _ state = update state