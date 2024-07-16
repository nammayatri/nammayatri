module Screens.RideSummaryScreen.Controller where

import Debug
import Prelude
import Screens.RideSummaryScreen.ScreenData
import Components.SourceToDestination.Controller as SourceToDestinationController
import PrestoDOM (Eval, continue, exit, update)
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)
import Screens (ScreenName(..), getScreen)
import Services.API
import Components.RideSummaryCard as RideSummaryCard
import Components.DropDownCard.Controller as DropDownCardController


instance showAction  ::  Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog



data Action = BackPressed
              | AcceptClick
              | NoAction
              | SourceToDestinationActionController SourceToDestinationController.Action
              | RideSummaryCardActionController RideSummaryCard.Action
              | PickUp DropDownCardController.Action 
              | IncludedCharges DropDownCardController.Action 
              | ExcludedCharges DropDownCardController.Action 
              | Terms DropDownCardController.Action 
              | ShowMapInterCity Number Number Number Number String String String
              | ShowMapRental Number Number String String String
              | ShowMapRegular Number Number Number Number String String String

data ScreenOutput = GoBack 
                    | AcceptScheduleRide String 
                    | UpdateRouteInterCity Number Number Number Number 
                    | UpdateRouteRental Number Number 
                    | UpdateRouteRegular Number Number Number Number

eval :: Action -> RideSummaryScreenState -> Eval Action ScreenOutput RideSummaryScreenState
eval BackPressed state = exit GoBack
eval AcceptClick state = do
                          let (BookingAPIEntity entity) = state.data.rideDetails
                              id = entity.id
                          exit $ AcceptScheduleRide id
eval (Terms DropDownCardController.OnClick) state = do
                                                      let old = state.props.termsAndConditionOpen
                                                      continue state{ props{termsAndConditionOpen = not old}}
eval (ExcludedCharges DropDownCardController.OnClick) state = do
                                                                let old = state.props.excludedChargesOpen
                                                                continue state{ props{excludedChargesOpen = not old}}
eval (IncludedCharges DropDownCardController.OnClick) state = do
                                                                let old = state.props.includedChargesOpen
                                                                continue state{ props{includedChargesOpen = not old}}
eval (PickUp DropDownCardController.OnClick) state = do
                                                      let old = state.props.pickUpOpen
                                                      continue state{ props{pickUpOpen = not old}}
eval (ShowMapInterCity slat slon dlat dlon key lat lon) state = exit $ UpdateRouteInterCity slat slon dlat dlon
eval (ShowMapRental slat slon key lat lon) state = exit $ UpdateRouteRental slat slon
eval (ShowMapRegular slat slon dlat dlon key lat lon) state = exit $ UpdateRouteRegular slat slon dlat dlon
eval _ state = continue state
