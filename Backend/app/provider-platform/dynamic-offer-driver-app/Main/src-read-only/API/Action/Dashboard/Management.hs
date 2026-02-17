{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management
  ( API,
    handler,
  )
where

import qualified API.Action.Dashboard.Management.Account
import qualified API.Action.Dashboard.Management.Booking
import qualified API.Action.Dashboard.Management.CoinsConfig
import qualified API.Action.Dashboard.Management.Driver
import qualified API.Action.Dashboard.Management.DriverCoins
import qualified API.Action.Dashboard.Management.DriverGoHome
import qualified API.Action.Dashboard.Management.DriverReferral
import qualified API.Action.Dashboard.Management.DriverRegistration
import qualified API.Action.Dashboard.Management.EntityInfo
import qualified API.Action.Dashboard.Management.Media
import qualified API.Action.Dashboard.Management.MediaFileDocument
import qualified API.Action.Dashboard.Management.Merchant
import qualified API.Action.Dashboard.Management.Message
import qualified API.Action.Dashboard.Management.NammaTag
import qualified API.Action.Dashboard.Management.Payout
import qualified API.Action.Dashboard.Management.PlanManagement
import qualified API.Action.Dashboard.Management.Revenue
import qualified API.Action.Dashboard.Management.Ride
import qualified API.Action.Dashboard.Management.SosMedia
import qualified API.Action.Dashboard.Management.System
import qualified API.Action.Dashboard.Management.VehicleInfo
import qualified API.Action.Dashboard.Management.Volunteer
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.Dashboard.Management.Account.API :<|> API.Action.Dashboard.Management.Booking.API :<|> API.Action.Dashboard.Management.CoinsConfig.API :<|> API.Action.Dashboard.Management.Driver.API :<|> API.Action.Dashboard.Management.DriverCoins.API :<|> API.Action.Dashboard.Management.DriverGoHome.API :<|> API.Action.Dashboard.Management.DriverReferral.API :<|> API.Action.Dashboard.Management.DriverRegistration.API :<|> API.Action.Dashboard.Management.EntityInfo.API :<|> API.Action.Dashboard.Management.Media.API :<|> API.Action.Dashboard.Management.MediaFileDocument.API :<|> API.Action.Dashboard.Management.Merchant.API :<|> API.Action.Dashboard.Management.Message.API :<|> API.Action.Dashboard.Management.NammaTag.API :<|> API.Action.Dashboard.Management.Payout.API :<|> API.Action.Dashboard.Management.PlanManagement.API :<|> API.Action.Dashboard.Management.Revenue.API :<|> API.Action.Dashboard.Management.Ride.API :<|> API.Action.Dashboard.Management.SosMedia.API :<|> API.Action.Dashboard.Management.System.API :<|> API.Action.Dashboard.Management.VehicleInfo.API :<|> API.Action.Dashboard.Management.Volunteer.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.Dashboard.Management.Account.handler merchantId city :<|> API.Action.Dashboard.Management.Booking.handler merchantId city :<|> API.Action.Dashboard.Management.CoinsConfig.handler merchantId city :<|> API.Action.Dashboard.Management.Driver.handler merchantId city :<|> API.Action.Dashboard.Management.DriverCoins.handler merchantId city :<|> API.Action.Dashboard.Management.DriverGoHome.handler merchantId city :<|> API.Action.Dashboard.Management.DriverReferral.handler merchantId city :<|> API.Action.Dashboard.Management.DriverRegistration.handler merchantId city :<|> API.Action.Dashboard.Management.EntityInfo.handler merchantId city :<|> API.Action.Dashboard.Management.Media.handler merchantId city :<|> API.Action.Dashboard.Management.MediaFileDocument.handler merchantId city :<|> API.Action.Dashboard.Management.Merchant.handler merchantId city :<|> API.Action.Dashboard.Management.Message.handler merchantId city :<|> API.Action.Dashboard.Management.NammaTag.handler merchantId city :<|> API.Action.Dashboard.Management.Payout.handler merchantId city :<|> API.Action.Dashboard.Management.PlanManagement.handler merchantId city :<|> API.Action.Dashboard.Management.Revenue.handler merchantId city :<|> API.Action.Dashboard.Management.Ride.handler merchantId city :<|> API.Action.Dashboard.Management.SosMedia.handler merchantId city :<|> API.Action.Dashboard.Management.System.handler merchantId city :<|> API.Action.Dashboard.Management.VehicleInfo.handler merchantId city :<|> API.Action.Dashboard.Management.Volunteer.handler merchantId city
