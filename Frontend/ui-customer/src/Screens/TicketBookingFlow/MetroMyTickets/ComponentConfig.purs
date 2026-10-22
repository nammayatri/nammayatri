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
import Helpers.FrfsUtils

getMetroLogoImage :: ST.MetroTicketCardData -> String ->  String
getMetroLogoImage ticketCard vehicleType = 
  let
    (API.FRFSTicketBookingStatusAPIRes resp) = ticketCard.metroTicketStatusApiResp
    city = getCityNameFromCode $ Just resp.city
    (CityMetroConfig config) = getMetroConfigFromCity city Nothing vehicleType
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
