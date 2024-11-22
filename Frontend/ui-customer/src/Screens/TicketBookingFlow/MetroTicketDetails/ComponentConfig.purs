{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingFlow.MetroTicketDetails.ComponentConfig where

import Prelude
import Data.String as DS
import Screens.Types as ST
import Data.Maybe(Maybe(..), maybe)
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton.Controller as PrimaryButton
import Components.SourceToDestination.Controller as SourceToDestination
import Types.App
import PrestoDOM 
import Language.Strings
import Language.Types
import Styles.Colors as Color
import Mobility.Prelude
import Font.Style as FontStyle
import PrestoDOM.Types.DomAttributes (Corners(..)) as PTD
import Data.Tuple(Tuple(..))
import Data.Array
import Debug
import Helpers.Utils (getMetroConfigFromCity, CityMetroConfig(..))
import Helpers.Utils as HU

newtype CancelBookingPopUpData = CancelBookingPopUpData { 
    title :: String
  , primaryText :: String
  , secondaryText :: String
  , endText :: String
  , buttonVisibility :: Boolean
}

newtype StatusPillConfig = StatusPillConfig {
    ticketStatus :: String
  , statusString :: String
  , statusPillColor :: String
}

cancelBookingButtonConfig :: ST.MetroTicketDetailsScreenState -> PrimaryButton.Config
cancelBookingButtonConfig state = 
  let
    config = PrimaryButton.config
    (CityMetroConfig cityMetroConfig) = getMetroConfigFromCity state.data.city Nothing ""
    (StatusPillConfig pillConfig) = getStatusPillConfig state
    cancelButtonVisibility = case pillConfig.ticketStatus of
      "ACTIVE" -> true
      _ -> false
    primaryButtonConfig' = config
      { textConfig
        { 
          textFromHtml = Just $ "<u>" <> (DS.replace (DS.Pattern "?") (DS.Replacement "") (getString CANCEL_BOOKING)) <> "</u>"
        , color = Color.black700
        , id = "cancelBookingButtonTextConfig"
        , textStyle = FontStyle.Body1
        }
      , background = Color.grey700
      , height     = V 40
      , isClickable  = true
      , cornerRadius = 0.0
      , margin = Margin 0 0 0 0
      , id = "cancelBookingButtonConfig"
      , visibility = boolToVisibility $ cancelButtonVisibility && cityMetroConfig.showCancelButton
      }
  in primaryButtonConfig'

cancelBookingPopupConfig :: ST.MetroTicketDetailsScreenState -> PopUpModal.Config
cancelBookingPopupConfig state =
  let
    config = PopUpModal.config
    (CancelBookingPopUpData popUpData) = getCancelBookingPopupData state
    config' = config
      {
        gravity = CENTER,
        margin = MarginHorizontal 24 24 ,
        buttonLayoutMargin = Margin 16 0 16 20 ,
        optionButtonOrientation = "VERTICAL",
        dismissPopup = true,
        backgroundClickable = true,
        primaryText {
          text = popUpData.title
        , visibility = boolToVisibility $ not $ DS.null popUpData.title
        , margin = Margin 16 24 16 8 
        , textStyle = FontStyle.Heading1
        },
        secondaryText {
          text = mkSecondaryTextForPopup (CancelBookingPopUpData popUpData)
        , margin = MarginBottom 2
        , textStyle = FontStyle.ParagraphText
        },
        option1 {
          text = getString YES_CANCEL_BOOKING
        , background = Color.black900
        , color = Color.yellow900
        , width = MATCH_PARENT
        , visibility = popUpData.buttonVisibility
        },
        option2 {
            text = getString GO_BACK_
          , color = Color.black650
          , background = Color.white900
          , strokeColor = Color.white900
          , width = MATCH_PARENT
          , margin = Margin 0 0 0 0
          , visibility = popUpData.buttonVisibility
        },
        optionWithHtml {
           textOpt1 {
              text = getString GOT_IT
            , color = Color.blue800
            , visibility = VISIBLE
            , textStyle = FontStyle.SubHeading2
           },
           visibility = not popUpData.buttonVisibility,
           isClickable = true,
           height = V 48,
           background = Color.white900,
           strokeColor = Color.white900
        },
        cornerRadius = (PTD.Corners 15.0 true true true true)
      }
  in config'

mkSecondaryTextForPopup :: CancelBookingPopUpData -> String
mkSecondaryTextForPopup (CancelBookingPopUpData popUpData) =
  let
    primaryText = popUpData.primaryText
    secondaryText = if DS.null popUpData.secondaryText then "" else "<br><b>" <> popUpData.secondaryText <> "</b>"
    endText = if DS.null popUpData.endText then "" else "<br><br>" <> popUpData.endText <> "<br>"
  in
    primaryText <> secondaryText <> endText

getCancelBookingPopupData :: ST.MetroTicketDetailsScreenState -> CancelBookingPopUpData 
getCancelBookingPopupData state = case state.props.isBookingCancellable of
  Just isCancellable -> if isCancellable then
    case state.props.refundAmount of
      Just amount -> if amount == 0.0 then
          mkPopupData (getString CANCEL_BOOKING) (getString BOOKINGS_WILL_BE_CANCELLED) (getString REFUND_NOT_APPLICABLE) (getString WOULD_YOU_LIKE_TO_PROCEED) true
        else
          mkPopupData (getString CANCEL_BOOKING) (getString $ BOOKINGS_WILL_BE_CANCELLED_WITH_REFUND (show amount)) "" (getString WOULD_YOU_LIKE_TO_PROCEED) true
      Nothing -> mkPopupData (getString CANCEL_BOOKING) (getString BOOKINGS_WILL_BE_CANCELLED) (getString REFUND_NOT_APPLICABLE) (getString WOULD_YOU_LIKE_TO_PROCEED) true
    else
      mkPopupData (getString TICKET_IS_NON_CANCELLABLE) (getString BOOKING_NOT_CANCELLABLE) "" "" false
  Nothing -> mkPopupData "" "" "" "" false
  where
  mkPopupData :: String -> String -> String -> String -> Boolean -> CancelBookingPopUpData
  mkPopupData title primaryText secondaryText endText buttonVisibility = 
    CancelBookingPopUpData  { 
      title
    , primaryText
    , secondaryText
    , endText
    , buttonVisibility
    }

getStatusPillConfig :: forall w . ST.MetroTicketDetailsScreenState -> StatusPillConfig
getStatusPillConfig state = 
  let 
    currentTicket = state.data.ticketsInfo !! state.props.currentTicketIndex
  in
    case currentTicket of 
      Just ticket ->  case ticket.status of
           "ACTIVE" -> mkStatusPillConfig ticket.status (getString ACTIVE_STR) Color.green900
           "EXPIRED" -> mkStatusPillConfig ticket.status (getString  EXPIRED_STR) Color.red900
           "USED" -> mkStatusPillConfig ticket.status (getString VERIFIED) Color.greyDavy 
           _ -> mkStatusPillConfig ticket.status ticket.status Color.grey900 
      Nothing -> mkStatusPillConfig "" "" Color.grey900 
  where
    mkStatusPillConfig ticketStatus statusString statusPillColor = 
      StatusPillConfig {
        ticketStatus,
        statusString,
        statusPillColor
      }

sourceToDestinationConfig :: ST.MetroTicketDetailsScreenState -> SourceToDestination.Config
sourceToDestinationConfig state =
  let
    config = SourceToDestination.config
    originStop = state.data.metroRoute !! 0
    destinationStop = state.data.metroRoute !! ((length state.data.metroRoute) - 1)
    sourceToDestinationConfig' =
      config
        { sourceImageConfig
          { imageUrl = HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_green_circle"
          , margin = MarginTop 3
          , width = V 8
          , height = V 8
          }
        , sourceTextConfig
          { text = maybe "" (\stop -> stop.name) originStop
          , margin = MarginHorizontal 16 15
          , color = Color.black500
          , ellipsize = true
          , maxLines = 1
          , textStyle = FontStyle.Body1
          }
        , destinationImageConfig
          { imageUrl = HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_red_circle"
          , margin = MarginTop 3
          , width = V 8
          , height = V 8
          }
        , destinationTextConfig
          { text = maybe "" (\stop -> stop.name) destinationStop
          , margin = MarginHorizontal 16 15
          , color = Color.black500
          , ellipsize = true
          , maxLines = 1
          , textStyle = FontStyle.Body1
          }
        }
  in
    sourceToDestinationConfig'