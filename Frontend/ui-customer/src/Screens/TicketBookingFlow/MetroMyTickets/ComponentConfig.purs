module Screens.TicketBookingFlow.MetroMyTickets.ComponentConfig where

import Prelude
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton 
import Components.PrimaryEditText as PrimaryEditText
import PrestoDOM
import Styles.Colors as Color
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Prelude ((<>))
import Common.Types.App(LazyCheck(..))
import Engineering.Helpers.Commons (getNewIDWithTag)
import Data.Maybe
import Font.Style as FontStyle
import JBridge as JB
import Screens.Types as ST
import Components.RequestInfoCard as InfoCard
import Language.Strings 
import Resources.LocalizableV2.Strings (getEN)
import Language.Types
import Services.API as API
import Helpers.Utils

newtype StatusConfig = StatusConfig 
  { status :: String
  , statusColor :: String
  , statusIcon :: String
  }

getTicketStatusConfig :: ST.MetroTicketCardData -> StatusConfig
getTicketStatusConfig ticketCard = 
  case ticketCard.status of
    "PAYMENT_PENDING" -> mkTicketStatusConfig (getString PENDING_STR) Color.yellow900 (fetchImage FF_COMMON_ASSET "ny_ic_yellow_clock")
    "CONFIRMING" -> mkTicketStatusConfig (getString CONFIRMING_STR) Color.red900 (fetchImage FF_COMMON_ASSET "ny_ic_yellow_clock")
    "FAILED" -> mkTicketStatusConfig (getString FAILED_STR) Color.red900 (fetchImage FF_COMMON_ASSET "ny_ic_red_triangle_warning")
    "CANCELLED" -> mkTicketStatusConfig (getString CANCELLED) Color.red900 (fetchImage FF_COMMON_ASSET "ny_ic_cross_red_outline")
    "CONFIRMED" -> mkTicketStatusConfig (getString CONFIRMED_STR) Color.black800 (fetchImage FF_COMMON_ASSET "ny_ic_green_tick")
    "EXPIRED" -> mkTicketStatusConfig (getString EXPIRED_STR) Color.black800 (fetchImage FF_ASSET "ny_ic_info")
    _ -> mkTicketStatusConfig "" "" ""
  where
    mkTicketStatusConfig status statusColor statusIcon = 
      StatusConfig { 
        status,
        statusColor,
        statusIcon
      }

getMetroLogoImage :: ST.MetroTicketCardData -> String
getMetroLogoImage ticketCard = 
  let
    (API.MetroTicketBookingStatus resp) = ticketCard.metroTicketStatusApiResp
    city = getCityNameFromCode $ Just resp.city
    (CityMetroConfig config) = getMetroConfigFromCity city Nothing
  in
    config.logoImage

bookTicketsButtonConfig :: PrimaryButton.Config
bookTicketsButtonConfig = 
 let
  config = PrimaryButton.config
  updateButtonConfig' = config 
   { 
    textConfig { 
     text = getString BOOK_TICKET
   }
   , height = (V 48)
   , cornerRadius = 8.0
   , margin = (Margin 16 0 16 0)
   }
  in updateButtonConfig'
