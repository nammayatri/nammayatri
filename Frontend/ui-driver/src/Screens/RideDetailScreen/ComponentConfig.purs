module Screens.RideDetailScreen.ComponentConfig where

import Common.Types.App
import Language.Strings
import Prelude
import PrestoDOM
import Components.RatingCard as RatingCard
import Screens.Types as ST
import Screens.Types (RideDetailScreenState)
import Components.PopUpModal as PopUpModal
import PrestoDOM.Types.DomAttributes (Corners(..))
import Language.Strings (getString)
import Language.Types (STR(..))


ratingCardViewState :: RideDetailScreenState -> RatingCard.RatingCardState
ratingCardViewState state = let
  config = RatingCard.config
  ratingCardConfig' = config {
    props
    {
        currentStage = false
      , showFareBreakUp = false
      , enableFeedback = true
      , isDriver = true
    }
    , ratingCardData  {
     driverName = state.data.customerName,
     rating = state.props.rating,
     feedback = state.props.feedback
    }
  }
  in ratingCardConfig'

callSupportConfig :: RideDetailScreenState -> PopUpModal.Config
callSupportConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    gravity = CENTER
  , cornerRadius = (Corners 15.0 true true true true)
  , margin = (Margin 16 0 16 0)
  , primaryText {
      text = getString CONTACT_SUPPORT <>"?"
    }
  , secondaryText {
      text = getString YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT
    }
    , option1 {
     text =  getString CANCEL
    }
  , option2 {
      text =  getString CALL_SUPPORT
    }
  }
  in popUpConfig'