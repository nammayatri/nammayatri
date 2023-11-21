module Screens.TicketBookingScreen.ComponentConfig where

import Prelude

import Common.Styles.Colors as Color
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Data.Maybe (Maybe(..))
import Font.Style (Style(..))
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), visibility)
import Screens.Types as ST
import Data.Array as DA
import JBridge as JB

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
                  ST.DescriptionStage -> "Zoological Garden, Alipore"
                  ST.ChooseTicketStage -> "Choose Tickets"
                  ST.ViewTicketStage -> if DA.null state.props.ticketBookingList.booked && DA.null state.props.ticketBookingList.pendingBooking then "My Tickets" else "Choose Tickets"
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
    primaryButtonConfig' = config
      { textConfig
        { text = (case state.props.currentStage of 
                    ST.DescriptionStage -> "Book Tickets"
                    ST.ChooseTicketStage -> ("Pay ₹" <> (show state.data.totalAmount))
                    ST.ViewTicketStage -> "Book Tickets"
                    _ -> "")
        , color = Color.yellow900
        }
      , cornerRadius = 8.0
      , background = Color.black900 
      , isClickable = (state.props.currentStage == ST.DescriptionStage) || (state.props.currentStage == ST.ViewTicketStage) || (state.props.termsAndConditionsSelected && state.data.totalAmount > 0 && state.props.validDate )
      , alpha = if (state.props.currentStage == ST.DescriptionStage) || (state.props.currentStage == ST.ViewTicketStage) || (state.props.termsAndConditionsSelected && state.data.totalAmount > 0 && state.props.validDate ) then 1.0 else 0.5
      , id = "BookTicketsButton"
      , margin = (MarginHorizontal 20 20)
      }
  in primaryButtonConfig'

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

shareTicketButtonConfig :: Boolean -> PrimaryButton.Config
shareTicketButtonConfig visibility' = PrimaryButton.config
  { textConfig 
    { text = "Share Tickets"
    , textStyle = Tags
    , weight = Just 1.0
    , gravity = CENTER
    , color = Color.black800
    }
  , height = WRAP_CONTENT
  , gravity = CENTER
  , visibility = if visibility' then VISIBLE else GONE
  , cornerRadius = 22.0
  , width = MATCH_PARENT
  , padding = Padding 16 11 16 11 
  , margin = Margin 10 10 10 0
  , isPrefixImage = true
  , stroke = "1," <> Color.grey700
  , background = Color.white900
  , prefixImageConfig
    { imageUrl = "ny_ic_share"
    , height = V 15
    , width = V 15
    , margin = MarginRight 5
    }
  , id = "ShareButton"
  }

refreshStatusButtonConfig :: ST.TicketBookingScreenState -> PrimaryButton.Config
refreshStatusButtonConfig state = PrimaryButton.config
    { textConfig 
      { text = "Refresh Status"
      , textStyle = Tags
      , weight = Just 1.0
      , gravity = CENTER
      , color = Color.black800
      }
      , height = WRAP_CONTENT
      , gravity = CENTER
      , cornerRadius = 32.0
      , width = MATCH_PARENT
      , padding =  Padding 0 13 0 13
      , margin = Margin 16 16 16 0
      , isPrefixImage = true
      , background = Color.white900
      , prefixImageConfig
        { imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_history_unfilled"
        , height = V 15
        , width = V 15
        , margin = MarginRight 5
        }
      , id = "RefershPaymentStatusButton"
    }

viewTicketButtonConfig :: String -> Boolean -> PrimaryButton.Config
viewTicketButtonConfig text visibility' = 
  PrimaryButton.config { 
  textConfig
      { text = text
      , color = Color.yellow900
      }
    , cornerRadius = 8.0
    , background = Color.black900 
    , visibility = if visibility' then VISIBLE else GONE
    , id = "ViewTicketsButton"
    , margin = (Margin 16 16 16 16)
    }
