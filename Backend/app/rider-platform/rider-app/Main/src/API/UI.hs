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

import qualified API.Action.UI.BBPS as BBPS
import qualified API.Action.UI.Cac as Cac
import qualified API.Action.UI.CustomerReferral as CustomerReferral
import qualified API.Action.UI.DeletedPerson as DeletedPerson
import qualified API.Action.UI.EditLocation as EditLocation
import qualified API.Action.UI.EstimateBP as EstimateBP
import qualified API.Action.UI.FRFSTicketService as FRFSTicketService
import qualified API.Action.UI.FavouriteDriver as FavouriteDriver
import qualified API.Action.UI.FollowRide as FollowRide
import qualified API.Action.UI.Invoice as Invoice
import qualified API.Action.UI.Miscellaneous as Miscellaneous
import qualified API.Action.UI.MultimodalConfirm as MultimodalConfirm
import qualified API.Action.UI.NearbyBuses as NearbyBuses
import qualified API.Action.UI.NearbyDrivers as NearbyDrivers
import qualified API.Action.UI.Places as Places
import qualified API.Action.UI.PriceBreakup as PriceBreakup
import qualified API.Action.UI.RidePayment as RidePayment
import qualified API.Action.UI.SocialLogin as SocialLogin
import qualified API.Action.UI.Sos as SosApi
import qualified API.Action.UI.TicketService as TicketService
import qualified API.Action.UI.TrackRoute as TrackRoute
import qualified API.Action.UI.TriggerFCM as TriggerFCM
import qualified API.Action.UI.Voip as Voip
import qualified API.UI.AadhaarVerification as AadhaarVerification
import qualified API.UI.AppInstalls as AppInstalls
import qualified API.UI.Booking as Booking
import qualified API.UI.Call as Call
import qualified API.UI.CallEvent as CallEvent
import qualified API.UI.Cancel as Cancel
import qualified API.UI.CancellationReason as CancellationReason
import qualified API.UI.Confirm as Confirm
import qualified API.UI.Disability as Disability
import qualified API.UI.FeedbackForm as FeedbackForm
import qualified API.UI.Frontend as Frontend
import qualified API.UI.GoogleTranslate as GoogleTranslateProxy
import qualified API.UI.HotSpot as HotSpot
import qualified API.UI.Issue as Issue
import qualified API.UI.Maps as MapsProxy
import qualified API.UI.PartnerOrganizationFRFS as PartnerOrgFRFS
import qualified API.UI.Payment as Payment
import qualified API.UI.PersonStats as PersonStats
import qualified API.UI.Profile as Profile
import qualified API.UI.Quote as Quote
import qualified API.UI.Rating as Rating
import qualified API.UI.Registration as Registration
import qualified API.UI.RentalsIntercityCache as RentalsIntercityCache
import qualified API.UI.Ride as Ride
import qualified API.UI.Route as Route
import qualified API.UI.SavedReqLocation as SavedReqLocation
import qualified API.UI.Search as Search
import qualified API.UI.Select as Select
import qualified API.UI.Serviceability as Serviceability
import qualified API.UI.Sos as Sos
import qualified API.UI.Support as Support
import qualified API.UI.Whatsapp as Whatsapp
import Environment
import EulerHS.Prelude
import Servant

type API =
  "v2"
    :> ( Get '[JSON] Text
           :<|> Registration.API
           :<|> Profile.API
           :<|> RidePayment.API
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
           :<|> MapsProxy.API
           :<|> GoogleTranslateProxy.API
           :<|> CancellationReason.API
           :<|> SavedReqLocation.API
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
           :<|> Invoice.API
           :<|> PriceBreakup.API
           :<|> FollowRide.API
           :<|> SosApi.API
           :<|> FRFSTicketService.API
           :<|> Cac.API
           :<|> CustomerReferral.API
           :<|> DeletedPerson.API
           :<|> EditLocation.API
           :<|> SocialLogin.API
           :<|> EstimateBP.API
           :<|> FavouriteDriver.API
           :<|> PartnerOrgFRFS.API
           :<|> TriggerFCM.API
           :<|> MultimodalConfirm.API
           :<|> TrackRoute.API
           :<|> BBPS.API
           :<|> Voip.API
           :<|> RentalsIntercityCache.API
           :<|> Miscellaneous.API
           :<|> NearbyDrivers.API
           :<|> NearbyBuses.API
           :<|> Places.API
       )

handler :: FlowServer API
handler =
  pure "App is UP"
    :<|> Registration.handler
    :<|> Profile.handler
    :<|> RidePayment.handler
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
    :<|> MapsProxy.handler
    :<|> GoogleTranslateProxy.handler
    :<|> CancellationReason.handler
    :<|> SavedReqLocation.handler
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
    :<|> Invoice.handler
    :<|> PriceBreakup.handler
    :<|> FollowRide.handler
    :<|> SosApi.handler
    :<|> FRFSTicketService.handler
    :<|> Cac.handler
    :<|> CustomerReferral.handler
    :<|> DeletedPerson.handler
    :<|> EditLocation.handler
    :<|> SocialLogin.handler
    :<|> EstimateBP.handler
    :<|> FavouriteDriver.handler
    :<|> PartnerOrgFRFS.handler
    :<|> TriggerFCM.handler
    :<|> MultimodalConfirm.handler
    :<|> TrackRoute.handler
    :<|> BBPS.handler
    :<|> Voip.handler
    :<|> RentalsIntercityCache.handler
    :<|> Miscellaneous.handler
    :<|> NearbyDrivers.handler
    :<|> NearbyBuses.handler
    :<|> Places.handler
