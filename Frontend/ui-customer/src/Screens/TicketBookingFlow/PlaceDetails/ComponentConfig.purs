module Screens.TicketBookingFlow.PlaceDetails.ComponentConfig where

import Prelude

import Common.Styles.Colors as Color
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Font.Style (Style(..))
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), visibility)
import Accessor (_name)
import Data.Lens ((^.))
import Screens.Types as ST
import Data.Array as DA
import JBridge as JB
import Services.API (TicketPlaceStatus (..), TicketPlaceResp(..))
import Language.Strings (getString )
import Language.Types (STR(..))

genericHeaderConfig :: ST.TicketBookingScreenState -> GenericHeader.Config
genericHeaderConfig state = let
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = fetchImage FF_ASSET "ny_ic_chevron_left"
      , height = V 25
      , width = V 25
      , margin = Margin 16 16 16 16
      } 
    , padding = PaddingVertical 5 5
    , textConfig {
        text = case state.props.currentStage of
                  ST.DescriptionStage -> case state.data.placeInfo of
                                            Just placeInfo -> placeInfo ^._name
                                            Nothing -> "Book Tickets"
                  ST.ChooseTicketStage -> "Choose Tickets"
                  ST.ViewTicketStage -> if DA.null state.props.ticketBookingList.booked && DA.null state.props.ticketBookingList.pendingBooking then (getString MY_TICKETS) else "Choose Tickets"
                  ST.TicketInfoStage -> state.props.selectedBookingInfo.ticketPlaceName
                  _ -> ""
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

primaryButtonConfig :: ST.TicketBookingScreenState -> PrimaryButton.Config
primaryButtonConfig state = let
    config = PrimaryButton.config
    buttonConfigForDescription = getButtonTextForDescriptionStage
    primaryButtonConfig' = config
      { textConfig
        { text = (case state.props.currentStage of 
                    ST.DescriptionStage -> buttonConfigForDescription.buttonText
                    ST.ChooseTicketStage -> ("Pay ₹" <> (show state.data.totalAmount))
                    ST.ViewTicketStage -> "Book Tickets"
                    _ -> "")
        , color = Color.yellow900
        }
      , cornerRadius = 8.0
      , background = Color.black900 
      , isClickable = (state.props.currentStage == ST.DescriptionStage && buttonConfigForDescription.isClickable) || (state.props.currentStage == ST.ViewTicketStage) || (state.props.termsAndConditionsSelected && state.data.totalAmount > 0 && state.props.validDate )
      , alpha = if (state.props.currentStage == ST.DescriptionStage && buttonConfigForDescription.isClickable) || (state.props.currentStage == ST.ViewTicketStage) || (state.props.termsAndConditionsSelected && state.data.totalAmount > 0 && state.props.validDate ) then 1.0 else 0.5
      , id = "BookTicketsButton"
      , margin = (MarginHorizontal 20 20)
      }
  in primaryButtonConfig'
  where
    getButtonTextForDescriptionStage :: {buttonText :: String, isClickable :: Boolean}
    getButtonTextForDescriptionStage =
      maybe  {buttonText : "Book Tickets", isClickable : true} 
        (\(TicketPlaceResp pInfo) ->  maybe {buttonText : "Book Tickets", isClickable : true}  
                      (\status -> case status of
                                    ComingSoon -> {buttonText : "Coming Soon", isClickable : false} 
                                    _ -> {buttonText : "Book Tickets", isClickable : true} 
                      ) pInfo.status
        ) state.data.placeInfo

primaryButtonConfig1 :: ST.TicketBookingScreenState -> PrimaryButton.Config
primaryButtonConfig1 state = 
  let config = PrimaryButton.config
      primaryButtonConfig' = config
        { textConfig
          { text = (case state.props.currentStage of 
                      ST.ChooseTicketStage -> ("Pay ₹" <> (show state.data.totalAmount))
                      _ -> "")
          , color = Color.yellow900
          }
        , cornerRadius = 8.0
        , background = Color.black900 
        , isClickable = (state.props.termsAndConditionsSelected && state.data.totalAmount > 0 && state.props.validDate)
        , alpha = if (state.props.termsAndConditionsSelected && state.data.totalAmount > 0 && state.props.validDate) then 1.0 else 0.5
        , id =  "PayTicketsButton"
        , enableLoader = JB.getBtnLoader "PayTicketsButton"
        }
  in primaryButtonConfig'
