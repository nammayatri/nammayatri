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

import qualified API.Action.UI.Cac as Cac
import qualified API.Action.UI.DemandHotspots as DemandHotspots
import qualified API.Action.UI.DriverOnboardingV2 as DriverOnboardingV2
import qualified API.Action.UI.DriverProfileQuestions as DriverProfileQuestions
import qualified API.Action.UI.EditBooking as EditBooking
import qualified API.Action.UI.FareCalculator as FareCalculator
import qualified API.Action.UI.LmsModule as LmsModule
import qualified API.Action.UI.MeterRide as MeterRide
import qualified API.Action.UI.OperationHub as OH
import qualified API.Action.UI.Operator as Operator
import qualified API.Action.UI.PriceBreakup as PriceBreakup
import qualified API.Action.UI.Reels as Reels
import qualified API.Action.UI.ReferralPayout as ReferralPayout
import qualified API.Action.UI.SocialLogin as SocialLogin
import qualified API.Action.UI.SpecialLocation as SpecialLocation
import qualified API.Action.UI.SpecialLocationWarrior as SpecialLocationWarrior
import qualified API.Action.UI.Tokenization as Tokenization
import qualified API.Action.UI.VehicleDetails as VehicleDetails
import qualified API.Action.UI.Voip as Voip
import qualified API.Action.UI.WMB as WMB
import qualified API.UI.Call as Call
import qualified API.UI.CallEvent as CallEvent
import qualified API.UI.CancellationReason as CancellationReason
import qualified API.UI.City as City
import qualified API.UI.Driver as Driver
import qualified API.UI.DriverCoins as DriverCoins
import qualified API.UI.DriverOnboarding as DriverOnboarding
import qualified API.UI.DriverProfileSummary as DriverProfileSummary
import qualified API.UI.DriverReferral as DriverReferral
import qualified API.UI.ExotelEndRide as ExotelEndRide
import qualified API.UI.Issue as Issue
import qualified API.UI.KioskLocation as KioskLocation
import qualified API.UI.LeaderBoard as LeaderBoard
import qualified API.UI.Maps as Maps
import qualified API.UI.Message as Message
import qualified API.UI.OnMessage as OnMessage
import qualified API.UI.OrgAdmin as OrgAdmin
import qualified API.UI.Payment as Payment
import qualified API.UI.Performance as Performance
import qualified API.UI.Plan as Plan
import qualified API.UI.Rating as Rating
import qualified API.UI.Registration as Registration
import qualified API.UI.Ride as Ride
import qualified API.UI.RideRoute as RideRoute
import qualified API.UI.RideSummary as RideSummary
import qualified API.UI.Route as Route
import qualified API.UI.Transporter as Transporter
import qualified API.UI.Whatsapp as Whatsapp
import Environment
import Kernel.Prelude
import Servant

type HealthCheckAPI = Get '[JSON] Text

type API =
  "ui"
    :> ( HealthCheckAPI
           :<|> Registration.API
           :<|> DemandHotspots.API
           :<|> DriverOnboarding.API
           :<|> DriverOnboardingV2.API
           :<|> DriverProfileQuestions.API
           :<|> OrgAdmin.API
           :<|> Payment.API
           :<|> Driver.API
           :<|> DriverProfileSummary.API
           :<|> Transporter.API
           :<|> Route.API
           :<|> Maps.API
           :<|> Ride.API
           :<|> Call.API
           :<|> CancellationReason.API
           :<|> Whatsapp.API
           :<|> Message.API
           :<|> Performance.API
           :<|> Rating.API
           :<|> DriverReferral.API
           :<|> Issue.API
           :<|> ExotelEndRide.API
           :<|> LeaderBoard.API
           :<|> OnMessage.API
           :<|> RideRoute.API
           :<|> CallEvent.API
           :<|> Plan.API
           :<|> KioskLocation.API
           :<|> DriverCoins.API
           :<|> RideSummary.API
           :<|> City.API
           :<|> LmsModule.API
           :<|> SpecialLocation.API
           :<|> Reels.API
           :<|> Cac.API
           :<|> EditBooking.API
           :<|> SocialLogin.API
           :<|> VehicleDetails.API
           :<|> PriceBreakup.API
           :<|> MeterRide.API
           :<|> Tokenization.API
           :<|> FareCalculator.API
           :<|> ReferralPayout.API
           :<|> SpecialLocationWarrior.API
           :<|> WMB.API
           :<|> Voip.API
           :<|> OH.API
           :<|> Operator.API
       )

handler :: FlowServer API
handler =
  pure "App is UP"
    :<|> Registration.handler
    :<|> DemandHotspots.handler
    :<|> DriverOnboarding.handler
    :<|> DriverOnboardingV2.handler
    :<|> DriverProfileQuestions.handler
    :<|> OrgAdmin.handler
    :<|> Payment.handler
    :<|> Driver.handler
    :<|> DriverProfileSummary.handler
    :<|> Transporter.handler
    :<|> Route.handler
    :<|> Maps.handler
    :<|> Ride.handler
    :<|> Call.handler
    :<|> CancellationReason.handler
    :<|> Whatsapp.handler
    :<|> Message.handler
    :<|> Performance.handler
    :<|> Rating.handler
    :<|> DriverReferral.handler
    :<|> Issue.handler
    :<|> ExotelEndRide.handler
    :<|> LeaderBoard.handler
    :<|> OnMessage.handler
    :<|> RideRoute.handler
    :<|> CallEvent.handler
    :<|> Plan.handler
    :<|> KioskLocation.handler
    :<|> DriverCoins.handler
    :<|> RideSummary.handler
    :<|> City.handler
    :<|> LmsModule.handler
    :<|> SpecialLocation.handler
    :<|> Reels.handler
    :<|> Cac.handler
    :<|> EditBooking.handler
    :<|> SocialLogin.handler
    :<|> VehicleDetails.handler
    :<|> PriceBreakup.handler
    :<|> MeterRide.handler
    :<|> Tokenization.handler
    :<|> FareCalculator.handler
    :<|> ReferralPayout.handler
    :<|> SpecialLocationWarrior.handler
    :<|> WMB.handler
    :<|> Voip.handler
    :<|> OH.handler
    :<|> Operator.handler
