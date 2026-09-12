{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management
  ( API,
    handler,
  )
where

import qualified API.Action.DashboardAuth.Management.Account
import qualified API.Action.DashboardAuth.Management.Booking
import qualified API.Action.DashboardAuth.Management.CancellationConsequence
import qualified API.Action.DashboardAuth.Management.CoinsConfig
import qualified API.Action.DashboardAuth.Management.Communication
import qualified API.Action.DashboardAuth.Management.DomainDiscountConfig
import qualified API.Action.DashboardAuth.Management.Driver
import qualified API.Action.DashboardAuth.Management.DriverCoins
import qualified API.Action.DashboardAuth.Management.DriverGoHome
import qualified API.Action.DashboardAuth.Management.DriverReferral
import qualified API.Action.DashboardAuth.Management.DriverRegistration
import qualified API.Action.DashboardAuth.Management.DriverVehicleQuality
import qualified API.Action.DashboardAuth.Management.EntityInfo
import qualified API.Action.DashboardAuth.Management.FarePolicyV2
import qualified API.Action.DashboardAuth.Management.FeedbackForm
import qualified API.Action.DashboardAuth.Management.FinanceManagement
import qualified API.Action.DashboardAuth.Management.GeohashArea
import qualified API.Action.DashboardAuth.Management.IncentiveJourney
import qualified API.Action.DashboardAuth.Management.KnowledgeCenter
import qualified API.Action.DashboardAuth.Management.Media
import qualified API.Action.DashboardAuth.Management.MediaFileDocument
import qualified API.Action.DashboardAuth.Management.Merchant
import qualified API.Action.DashboardAuth.Management.Message
import qualified API.Action.DashboardAuth.Management.NammaTag
import qualified API.Action.DashboardAuth.Management.Notification
import qualified API.Action.DashboardAuth.Management.Payout
import qualified API.Action.DashboardAuth.Management.PlanManagement
import qualified API.Action.DashboardAuth.Management.Pricing
import qualified API.Action.DashboardAuth.Management.Revenue
import qualified API.Action.DashboardAuth.Management.Ride
import qualified API.Action.DashboardAuth.Management.ScheduledBooking
import qualified API.Action.DashboardAuth.Management.SearchTry
import qualified API.Action.DashboardAuth.Management.SosMedia
import qualified API.Action.DashboardAuth.Management.SpecialZoneQueue
import qualified API.Action.DashboardAuth.Management.System
import qualified API.Action.DashboardAuth.Management.Vehicle
import qualified API.Action.DashboardAuth.Management.VehicleDetails
import qualified API.Action.DashboardAuth.Management.VehicleInfo
import qualified API.Action.DashboardAuth.Management.Volunteer
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.DashboardAuth.Management.Account.API :<|> API.Action.DashboardAuth.Management.Booking.API :<|> API.Action.DashboardAuth.Management.CancellationConsequence.API :<|> API.Action.DashboardAuth.Management.CoinsConfig.API :<|> API.Action.DashboardAuth.Management.Communication.API :<|> API.Action.DashboardAuth.Management.DomainDiscountConfig.API :<|> API.Action.DashboardAuth.Management.Driver.API :<|> API.Action.DashboardAuth.Management.DriverCoins.API :<|> API.Action.DashboardAuth.Management.DriverGoHome.API :<|> API.Action.DashboardAuth.Management.DriverReferral.API :<|> API.Action.DashboardAuth.Management.DriverRegistration.API :<|> API.Action.DashboardAuth.Management.DriverVehicleQuality.API :<|> API.Action.DashboardAuth.Management.EntityInfo.API :<|> API.Action.DashboardAuth.Management.FarePolicyV2.API :<|> API.Action.DashboardAuth.Management.FeedbackForm.API :<|> API.Action.DashboardAuth.Management.FinanceManagement.API :<|> API.Action.DashboardAuth.Management.GeohashArea.API :<|> API.Action.DashboardAuth.Management.IncentiveJourney.API :<|> API.Action.DashboardAuth.Management.KnowledgeCenter.API :<|> API.Action.DashboardAuth.Management.Media.API :<|> API.Action.DashboardAuth.Management.MediaFileDocument.API :<|> API.Action.DashboardAuth.Management.Merchant.API :<|> API.Action.DashboardAuth.Management.Message.API :<|> API.Action.DashboardAuth.Management.NammaTag.API :<|> API.Action.DashboardAuth.Management.Notification.API :<|> API.Action.DashboardAuth.Management.Payout.API :<|> API.Action.DashboardAuth.Management.PlanManagement.API :<|> API.Action.DashboardAuth.Management.Pricing.API :<|> API.Action.DashboardAuth.Management.Revenue.API :<|> API.Action.DashboardAuth.Management.Ride.API :<|> API.Action.DashboardAuth.Management.ScheduledBooking.API :<|> API.Action.DashboardAuth.Management.SearchTry.API :<|> API.Action.DashboardAuth.Management.SosMedia.API :<|> API.Action.DashboardAuth.Management.SpecialZoneQueue.API :<|> API.Action.DashboardAuth.Management.System.API :<|> API.Action.DashboardAuth.Management.Vehicle.API :<|> API.Action.DashboardAuth.Management.VehicleDetails.API :<|> API.Action.DashboardAuth.Management.VehicleInfo.API :<|> API.Action.DashboardAuth.Management.Volunteer.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.DashboardAuth.Management.Account.handler merchantId city :<|> API.Action.DashboardAuth.Management.Booking.handler merchantId city :<|> API.Action.DashboardAuth.Management.CancellationConsequence.handler merchantId city :<|> API.Action.DashboardAuth.Management.CoinsConfig.handler merchantId city :<|> API.Action.DashboardAuth.Management.Communication.handler merchantId city :<|> API.Action.DashboardAuth.Management.DomainDiscountConfig.handler merchantId city :<|> API.Action.DashboardAuth.Management.Driver.handler merchantId city :<|> API.Action.DashboardAuth.Management.DriverCoins.handler merchantId city :<|> API.Action.DashboardAuth.Management.DriverGoHome.handler merchantId city :<|> API.Action.DashboardAuth.Management.DriverReferral.handler merchantId city :<|> API.Action.DashboardAuth.Management.DriverRegistration.handler merchantId city :<|> API.Action.DashboardAuth.Management.DriverVehicleQuality.handler merchantId city :<|> API.Action.DashboardAuth.Management.EntityInfo.handler merchantId city :<|> API.Action.DashboardAuth.Management.FarePolicyV2.handler merchantId city :<|> API.Action.DashboardAuth.Management.FeedbackForm.handler merchantId city :<|> API.Action.DashboardAuth.Management.FinanceManagement.handler merchantId city :<|> API.Action.DashboardAuth.Management.GeohashArea.handler merchantId city :<|> API.Action.DashboardAuth.Management.IncentiveJourney.handler merchantId city :<|> API.Action.DashboardAuth.Management.KnowledgeCenter.handler merchantId city :<|> API.Action.DashboardAuth.Management.Media.handler merchantId city :<|> API.Action.DashboardAuth.Management.MediaFileDocument.handler merchantId city :<|> API.Action.DashboardAuth.Management.Merchant.handler merchantId city :<|> API.Action.DashboardAuth.Management.Message.handler merchantId city :<|> API.Action.DashboardAuth.Management.NammaTag.handler merchantId city :<|> API.Action.DashboardAuth.Management.Notification.handler merchantId city :<|> API.Action.DashboardAuth.Management.Payout.handler merchantId city :<|> API.Action.DashboardAuth.Management.PlanManagement.handler merchantId city :<|> API.Action.DashboardAuth.Management.Pricing.handler merchantId city :<|> API.Action.DashboardAuth.Management.Revenue.handler merchantId city :<|> API.Action.DashboardAuth.Management.Ride.handler merchantId city :<|> API.Action.DashboardAuth.Management.ScheduledBooking.handler merchantId city :<|> API.Action.DashboardAuth.Management.SearchTry.handler merchantId city :<|> API.Action.DashboardAuth.Management.SosMedia.handler merchantId city :<|> API.Action.DashboardAuth.Management.SpecialZoneQueue.handler merchantId city :<|> API.Action.DashboardAuth.Management.System.handler merchantId city :<|> API.Action.DashboardAuth.Management.Vehicle.handler merchantId city :<|> API.Action.DashboardAuth.Management.VehicleDetails.handler merchantId city :<|> API.Action.DashboardAuth.Management.VehicleInfo.handler merchantId city :<|> API.Action.DashboardAuth.Management.Volunteer.handler merchantId city
