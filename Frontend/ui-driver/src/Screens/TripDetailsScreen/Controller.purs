{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TripDetailsScreen.Controller where

import Prelude (class Show, pure, unit, not, bind, ($), discard, show, (<>), void)
import Screens.Types as ST
import PrestoDOM.Types.Core (class Loggable)
import PrestoDOM (Eval, update, exit, continue, continueWithCmd)
import Components.PrimaryButton as PrimaryButton
import Components.GenericHeader as GenericHeader
import Components.SourceToDestination as SourceToDestination
import JBridge (copyToClipboard, toast)
import Language.Types(STR(..))
import Language.Strings (getString)
import JBridge (hideKeyboardOnNavigation)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Helpers.Utils as HU
import Common.Types.App as App
import Data.Maybe as Mb

instance showAction :: Show Action where
  show (PrimaryButtonActionController _ var1) = "PrimaryButtonActionController_" <> show var1
  show (GenericHeaderActionController var1) = "GenericHeaderActionController_" <> show var1
  show (SourceToDestinationActionController var1) = "SourceToDestinationActionController_" <> show var1
  show (BackPressed ) = "BackPressed"
  show (ReportIssue ) = "ReportIssue"
  show (MessageTextChanged _) = "MessageTextChanged"
  show (Copy ) = "Copy"
  show (HelpAndSupport ) = "HelpAndSupport"
  show (NoAction ) = "NoAction"
  show (AfterRender ) = "AfterRender"

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
        AfterRender -> trackAppScreenRender appId "screen" (getScreen TRIP_DETAILS_SCREEN)
        BackPressed -> do
            trackAppBackPress appId (getScreen TRIP_DETAILS_SCREEN)
            trackAppEndScreen appId (getScreen TRIP_DETAILS_SCREEN)
        PrimaryButtonActionController primaryButtonState act -> case primaryButtonState.props.issueReported of
            true -> case act of
                PrimaryButton.OnClick -> do
                    trackAppActionClick appId (getScreen TRIP_DETAILS_SCREEN) "primary_button" "go_home_on_click"
                    trackAppEndScreen appId (getScreen TRIP_DETAILS_SCREEN)
                PrimaryButton.NoAction -> trackAppActionClick appId (getScreen TRIP_DETAILS_SCREEN) "primary_button" "go_home_no_action"
            false -> case act of
                PrimaryButton.OnClick -> do
                    trackAppActionClick appId (getScreen TRIP_DETAILS_SCREEN) "primary_button" "submit_on_click"
                    trackAppEndScreen appId (getScreen TRIP_DETAILS_SCREEN)
                PrimaryButton.NoAction -> trackAppActionClick appId (getScreen TRIP_DETAILS_SCREEN) "primary_button" "submit_no_action"
        GenericHeaderActionController act -> case act of
            GenericHeader.PrefixImgOnClick -> do
                trackAppActionClick appId (getScreen TRIP_DETAILS_SCREEN) "generic_header" "back_icon_on_click"
                trackAppEndScreen appId (getScreen TRIP_DETAILS_SCREEN)
            GenericHeader.SuffixImgOnClick -> do
                trackAppActionClick appId (getScreen TRIP_DETAILS_SCREEN) "generic_header" "forward_icon_on_click"
                trackAppEndScreen appId (getScreen TRIP_DETAILS_SCREEN)
        ReportIssue -> trackAppActionClick appId (getScreen TRIP_DETAILS_SCREEN) "in_screen" "report_issue"
        MessageTextChanged str -> trackAppActionClick appId (getScreen TRIP_DETAILS_SCREEN) "in_screen" "message_text_changed"
        Copy -> trackAppActionClick appId (getScreen TRIP_DETAILS_SCREEN) "in_screen" "copied"
        HelpAndSupport -> trackAppActionClick appId (getScreen TRIP_DETAILS_SCREEN) "in_screen" "help_and_support_onclick"
        SourceToDestinationActionController action -> trackAppScreenEvent appId (getScreen TRIP_DETAILS_SCREEN) "in_screen" "source_to_destination_action"
        NoAction -> trackAppScreenEvent appId (getScreen TRIP_DETAILS_SCREEN) "in_screen" "no_action"

data Action = PrimaryButtonActionController ST.TripDetailsScreenState PrimaryButton.Action
            | GenericHeaderActionController GenericHeader.Action
            | SourceToDestinationActionController SourceToDestination.Action
            | BackPressed
            | ReportIssue
            | MessageTextChanged String
            | Copy
            | HelpAndSupport
            | NoAction
            | AfterRender
data ScreenOutput = OnSubmit | GoToEarning | GoToHelpAndSupport | GoToHome

eval :: Action -> ST.TripDetailsScreenState -> Eval Action ScreenOutput ST.TripDetailsScreenState

eval AfterRender state = continue state

eval BackPressed state = 
    case state.data.goBackTo of
        ST.Home ->  if (HU.isParentView App.FunctionCall) 
                    then do 
                        void $ pure $ HU.emitTerminateApp Mb.Nothing true
                        exit GoToHome
                    else continue state
        ST.Earning -> exit GoToEarning

eval ReportIssue state = continue state { props { reportIssue = not state.props.reportIssue}}

eval (MessageTextChanged a) state =  continue state { data { message = a }}

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick )) state = continueWithCmd state [do pure BackPressed]

eval (PrimaryButtonActionController primaryButtonState PrimaryButton.OnClick) state = do
    _ <- pure $ hideKeyboardOnNavigation true
    if state.props.issueReported then exit GoToEarning
    else continue state{props{issueReported = true}}

eval Copy state = continueWithCmd state [ do
    _ <- pure $ copyToClipboard state.data.tripId
    _ <- pure $ toast (getString COPIED)
    pure NoAction
  ]
eval HelpAndSupport state = exit $ GoToHelpAndSupport

eval _ state = update state