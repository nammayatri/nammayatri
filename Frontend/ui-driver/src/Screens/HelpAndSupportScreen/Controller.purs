module Screens.HelpAndSupportScreen.Controller where

import Prelude (class Show, pure, unit, ($), discard, bind)
import PrestoDOM (Eval, continue, exit)
import Screens.Types (HelpAndSupportScreenState)
import PrestoDOM.Types.Core (class Loggable)
import Components.SourceToDestination as SourceToDestinationController
import Screens.HelpAndSupportScreen.ScreenData(ListOptions(..))
import Language.Strings (getString)
import Services.APITypes (GetRidesHistoryResp)
import Language.Types(STR(..))
import Services.Config (getSupportNumber)
import JBridge (showDialer)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen HELP_AND_SUPPORT_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen HELP_AND_SUPPORT_SCREEN)
      trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
    SourceToDestinationAction (SourceToDestinationController.Dummy) -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "source_to_destination" "dummy"
    OptionClick optionIndex -> do
      trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "view_options"
      trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
    ViewAllRides -> do
      trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "view_rides"
      trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
    ReportIssue -> do
      trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "report_issue"
      trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
    RideHistoryAPIResponse resp -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "ride_history_api_resp"
    NoRidesAction -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "no_action_view_rides"
    NoAction -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "no_action"


data ScreenOutput = GoBack 
                  | GoToWriteToUsScreen 
                  | GoToTripDetailsScreen HelpAndSupportScreenState
                  | GoToMyRidesScreen

data Action = NoAction
             | BackPressed 
             | SourceToDestinationAction SourceToDestinationController.Action 
             | OptionClick ListOptions 
             | ReportIssue 
             | ViewAllRides 
             | RideHistoryAPIResponse GetRidesHistoryResp
             | AfterRender
             | NoRidesAction


eval :: Action -> HelpAndSupportScreenState -> Eval Action ScreenOutput HelpAndSupportScreenState
eval AfterRender state = continue state
eval BackPressed state = exit GoBack
eval (SourceToDestinationAction (SourceToDestinationController.Dummy)) state = continue state
eval ReportIssue state = exit $ GoToTripDetailsScreen state
eval ViewAllRides state = exit $ GoToMyRidesScreen

eval (OptionClick optionIndex) state = do
  case optionIndex of
    GettingStartedFaq -> continue state
    OtherIssues -> exit $ GoToWriteToUsScreen
    CallSupportCenter -> do
      _ <- pure $ showDialer (getSupportNumber "")
      continue state
eval _ state = continue state

getTitle :: ListOptions -> String
getTitle menuOption = 
  case menuOption of
    GettingStartedFaq -> (getString GETTING_STARTED_AND_FAQ)
    OtherIssues -> (getString FOR_OTHER_ISSUES_WRITE_TO_US)
    CallSupportCenter -> (getString CALL_SUPPORT_CENTER)
