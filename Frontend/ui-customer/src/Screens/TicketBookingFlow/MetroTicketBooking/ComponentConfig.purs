module Screens.TicketBookingFlow.MetroTicketBooking.ComponentConfig where

import Prelude
import Components.GenericHeader as GenericHeader
-- import Components.SelectionTabModal as SelectionTabModal
import Components.PrimaryButton as PrimaryButton 
import Components.PrimaryEditText as PrimaryEditText
-- import Components.IncrementDecrementModel as IncrementDecrementModel
import PrestoDOM --(Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST 
import Styles.Colors as Color
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Prelude ((<>))
import Common.Types.App(LazyCheck(..))
import Engineering.Helpers.Commons (getNewIDWithTag)
import Data.Maybe
import Font.Style as FontStyle
import JBridge as JB

metroTicketBookingHeaderConfig :: ST.MetroTicketBookingScreenState -> GenericHeader.Config
metroTicketBookingHeaderConfig state = let
    config = GenericHeader.config
    genericHeaderConfig' = config 
        {
          height = WRAP_CONTENT
        , width = WRAP_CONTENT
        , prefixImageConfig {
           visibility = VISIBLE
          , imageUrl = fetchImage FF_ASSET "ny_ic_chevron_left"
          , height = V 25
          , width = V 25
          , margin = Margin 16 16 16 16
          } 
        , padding = PaddingVertical 5 5
        , textConfig {
            text = "Buy Metro Tickets"
          , color = Color.darkCharcoal
          }
        , suffixImageConfig {
            visibility = GONE
          }
        }
    in genericHeaderConfig'

metroSrcEditText :: ST.MetroTicketBookingScreenState -> PrimaryEditText.Config
metroSrcEditText state = PrimaryEditText.config { 
      id = getNewIDWithTag "EnterAadhaarOTPEditText"
      , editText
        { placeholder = "Starting From?"
        , singleLine = true
        , gravity = CENTER_VERTICAL
        , pattern = Just "[0-9]*,6"
        , textStyle = FontStyle.SubHeading1
        -- , letterSpacing = if state.data.otp == "" then PX 0.0 else PX 4.0
        }
      , topLabel{
        visibility = VISIBLE
      , text = "From"
      , textStyle = FontStyle.Body3
      , color = Color.black900
      }
      , margin = Margin 0 0 0 0
      , type = "number"
      }

metroDestEditText :: ST.MetroTicketBookingScreenState -> PrimaryEditText.Config
metroDestEditText state = PrimaryEditText.config { 
      id = getNewIDWithTag "EnterAadhaarOTPEditText"
      , editText
        { placeholder = "Where to?"
        , singleLine = true
        , gravity = CENTER_VERTICAL
        , pattern = Just "[0-9]*,6"
        , textStyle = FontStyle.SubHeading1
        -- , letterSpacing = if state.data.otp == "" then PX 0.0 else PX 4.0
        }
      , topLabel{
        visibility = VISIBLE
      , text = "To"
      , textStyle = FontStyle.Body3
      , color = Color.black900
      }
      , margin = Margin 0 0 0 0
      , type = "number"
      }


updateButtonConfig :: ST.MetroTicketBookingScreenState -> PrimaryButton.Config
updateButtonConfig state = let
    config = PrimaryButton.config
    updateButtonConfig' = config 
        { textConfig{ text = if state.data.ticketPrice /= 0 then ("Pay " <> (show state.data.ticketPrice) <> "₹")  else "Get Fare"}--, color = state.data.config.primaryTextColor
        -- , accessibilityHint = if state.props.isBtnEnabled then "Update Personal Details Button " else if not state.props.isEmailValid then "Update Button Is DisAbled : Please Enter A Valid Email" else if (not state.props.isNameValid) then  "Update Button Is DisAbled : Please Enter A Valid Name" else "Update Button Is DisAbled"}
        , height = (V 48)
        , cornerRadius = 8.0
        , margin = (Margin 16 0 16 0)
        , id = "PrimaryButtonUpdate"
        , enableLoader = (JB.getBtnLoader "PrimaryButtonUpdate")
        , isClickable = state.props.isButtonActive
        , alpha = if state.props.isButtonActive then 1.0 else 0.5
        -- , background = state.data.config.primaryBackground
        }
    in updateButtonConfig'
