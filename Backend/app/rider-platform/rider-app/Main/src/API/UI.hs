{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI
  ( API,
    handler,
  )
where

import qualified API.UI.AadhaarVerification as AadhaarVerification
import qualified API.UI.AppInstalls as AppInstalls
import qualified API.UI.Booking as Booking
import qualified API.UI.Call as Call
import qualified API.UI.CallEvent as CallEvent
import qualified API.UI.Cancel as Cancel
import qualified API.UI.CancellationReason as CancellationReason
import qualified API.UI.Confirm as Confirm
import qualified API.UI.CustomerSupport as CustomerSupport
import qualified API.UI.Disability as Disability
import qualified API.UI.FeedbackForm as FeedbackForm
import qualified API.UI.Frontend as Frontend
import qualified API.UI.GoogleTranslate as GoogleTranslateProxy
import qualified API.UI.HotSpot as HotSpot
import qualified API.UI.Issue as Issue
import qualified API.UI.Maps as MapsProxy
import qualified API.UI.Payment as Payment
import qualified API.UI.PersonStats as PersonStats
import qualified API.UI.Profile as Profile
import qualified API.UI.Quote as Quote
import qualified API.UI.Rating as Rating
import qualified API.UI.Registration as Registration
import qualified API.UI.Ride as Ride
import qualified API.UI.Route as Route
import qualified API.UI.SavedReqLocation as SavedReqLocation
import qualified API.UI.Search as Search
import qualified API.UI.Select as Select
import qualified API.UI.Serviceability as Serviceability
import qualified API.UI.Sos as Sos
import qualified API.UI.Support as Support
import qualified API.UI.TicketService as TicketService
import qualified API.UI.Webengage.InfoBIPWebhook as InfoBIPWebhook
import qualified API.UI.Webengage.Webengage as Webengage
import qualified API.UI.Whatsapp as Whatsapp
import Environment
import EulerHS.Prelude
import Servant

type API =
  "v2"
    :> ( Get '[JSON] Text
           :<|> Registration.API
           :<|> Profile.API
           :<|> Payment.API
           :<|> Search.API
           :<|> Select.API
           :<|> Quote.API
           :<|> Confirm.API
           :<|> Booking.API
           :<|> Cancel.API
           :<|> Ride.API
           :<|> Call.API
           :<|> Support.API
           :<|> Route.API
           :<|> Serviceability.API
           :<|> Rating.API
           :<|> FeedbackForm.API
           :<|> CustomerSupport.API
           :<|> MapsProxy.API
           :<|> GoogleTranslateProxy.API
           :<|> CancellationReason.API
           :<|> SavedReqLocation.API
           :<|> Webengage.API
           :<|> InfoBIPWebhook.API
           :<|> Frontend.API
           :<|> Whatsapp.API
           :<|> Sos.API
           :<|> CallEvent.API
           :<|> AppInstalls.API
           :<|> PersonStats.API
           :<|> HotSpot.API
           :<|> Disability.API
           :<|> AadhaarVerification.API
           :<|> Issue.API
           :<|> TicketService.API
       )

handler :: FlowServer API
handler =
  pure "App is UP"
    :<|> Registration.handler
    :<|> Profile.handler
    :<|> Payment.handler
    :<|> Search.handler
    :<|> Select.handler
    :<|> Quote.handler
    :<|> Confirm.handler
    :<|> Booking.handler
    :<|> Cancel.handler
    :<|> Ride.handler
    :<|> Call.handler
    :<|> Support.handler
    :<|> Route.handler
    :<|> Serviceability.handler
    :<|> Rating.handler
    :<|> FeedbackForm.handler
    :<|> CustomerSupport.handler
    :<|> MapsProxy.handler
    :<|> GoogleTranslateProxy.handler
    :<|> CancellationReason.handler
    :<|> SavedReqLocation.handler
    :<|> Webengage.handler
    :<|> InfoBIPWebhook.handler
    :<|> Frontend.handler
    :<|> Whatsapp.handler
    :<|> Sos.handler
    :<|> CallEvent.handler
    :<|> AppInstalls.handler
    :<|> PersonStats.handler
    :<|> HotSpot.handler
    :<|> Disability.handler
    :<|> AadhaarVerification.handler
    :<|> Issue.handler
    :<|> TicketService.handler
