module Screens.TicketBookingFlow.MetroTicketDetails.ComponentConfig where

import Prelude
import Common.Types.App
import Engineering.Helpers.Commons


getShareTicketConfig :: ShareImageConfig 
getShareTicketConfig  = {
    viewId : getNewIDWithTag "metro_ticket_details_view"
  , code : ""
  , logoId : ""
  , isReferral : false
}