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

import qualified API.UI.Booking as Booking
import qualified API.UI.Call as Call
import qualified API.UI.Cancel as Cancel
import qualified API.UI.CancellationReason as CancellationReason
import qualified API.UI.Confirm as Confirm
import qualified API.UI.CustomerSupport as CustomerSupport
import qualified API.UI.Feedback as Feedback
import qualified API.UI.Frontend as Frontend
import qualified API.UI.GoogleTranslate as GoogleTranslateProxy
import qualified API.UI.Maps as MapsProxy
import qualified API.UI.Profile as Profile
import qualified API.UI.Quote as Quote
import qualified API.UI.Registration as Registration
import qualified API.UI.Ride as Ride
import qualified API.UI.Route as Route
import qualified API.UI.SavedReqLocation as SavedReqLocation
import qualified API.UI.Search as Search
import qualified API.UI.Select as Select
import qualified API.UI.Serviceability as Serviceability
import qualified API.UI.Sos as Sos
import qualified API.UI.Support as Support
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
           :<|> Feedback.API
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
       )

handler :: FlowServer API
handler =
  pure "App is UP"
    :<|> Registration.handler
    :<|> Profile.handler
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
    :<|> Feedback.handler
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
