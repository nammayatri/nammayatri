{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management where

import qualified API.Action.ProviderPlatform.Management.Account
import qualified API.Action.ProviderPlatform.Management.Booking
import qualified API.Action.ProviderPlatform.Management.CoinsConfig
import qualified API.Action.ProviderPlatform.Management.Driver
import qualified API.Action.ProviderPlatform.Management.DriverCoins
import qualified API.Action.ProviderPlatform.Management.DriverGoHome
import qualified API.Action.ProviderPlatform.Management.DriverReferral
import qualified API.Action.ProviderPlatform.Management.DriverRegistration
import qualified API.Action.ProviderPlatform.Management.EntityInfo
import qualified API.Action.ProviderPlatform.Management.Media
import qualified API.Action.ProviderPlatform.Management.MediaFileDocument
import qualified API.Action.ProviderPlatform.Management.Merchant
import qualified API.Action.ProviderPlatform.Management.Message
import qualified API.Action.ProviderPlatform.Management.NammaTag
import qualified API.Action.ProviderPlatform.Management.Payout
import qualified API.Action.ProviderPlatform.Management.PlanManagement
import qualified API.Action.ProviderPlatform.Management.Revenue
import qualified API.Action.ProviderPlatform.Management.Ride
import qualified API.Action.ProviderPlatform.Management.System
import qualified API.Action.ProviderPlatform.Management.VehicleInfo
import qualified API.Action.ProviderPlatform.Management.Volunteer
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.ProviderPlatform.Management.Account.API :<|> API.Action.ProviderPlatform.Management.Booking.API :<|> API.Action.ProviderPlatform.Management.CoinsConfig.API :<|> API.Action.ProviderPlatform.Management.Driver.API :<|> API.Action.ProviderPlatform.Management.DriverCoins.API :<|> API.Action.ProviderPlatform.Management.DriverGoHome.API :<|> API.Action.ProviderPlatform.Management.DriverReferral.API :<|> API.Action.ProviderPlatform.Management.DriverRegistration.API :<|> API.Action.ProviderPlatform.Management.EntityInfo.API :<|> API.Action.ProviderPlatform.Management.Media.API :<|> API.Action.ProviderPlatform.Management.MediaFileDocument.API :<|> API.Action.ProviderPlatform.Management.Merchant.API :<|> API.Action.ProviderPlatform.Management.Message.API :<|> API.Action.ProviderPlatform.Management.NammaTag.API :<|> API.Action.ProviderPlatform.Management.Payout.API :<|> API.Action.ProviderPlatform.Management.PlanManagement.API :<|> API.Action.ProviderPlatform.Management.Revenue.API :<|> API.Action.ProviderPlatform.Management.Ride.API :<|> API.Action.ProviderPlatform.Management.System.API :<|> API.Action.ProviderPlatform.Management.VehicleInfo.API :<|> API.Action.ProviderPlatform.Management.Volunteer.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.ProviderPlatform.Management.Account.handler merchantId city :<|> API.Action.ProviderPlatform.Management.Booking.handler merchantId city :<|> API.Action.ProviderPlatform.Management.CoinsConfig.handler merchantId city :<|> API.Action.ProviderPlatform.Management.Driver.handler merchantId city :<|> API.Action.ProviderPlatform.Management.DriverCoins.handler merchantId city :<|> API.Action.ProviderPlatform.Management.DriverGoHome.handler merchantId city :<|> API.Action.ProviderPlatform.Management.DriverReferral.handler merchantId city :<|> API.Action.ProviderPlatform.Management.DriverRegistration.handler merchantId city :<|> API.Action.ProviderPlatform.Management.EntityInfo.handler merchantId city :<|> API.Action.ProviderPlatform.Management.Media.handler merchantId city :<|> API.Action.ProviderPlatform.Management.MediaFileDocument.handler merchantId city :<|> API.Action.ProviderPlatform.Management.Merchant.handler merchantId city :<|> API.Action.ProviderPlatform.Management.Message.handler merchantId city :<|> API.Action.ProviderPlatform.Management.NammaTag.handler merchantId city :<|> API.Action.ProviderPlatform.Management.Payout.handler merchantId city :<|> API.Action.ProviderPlatform.Management.PlanManagement.handler merchantId city :<|> API.Action.ProviderPlatform.Management.Revenue.handler merchantId city :<|> API.Action.ProviderPlatform.Management.Ride.handler merchantId city :<|> API.Action.ProviderPlatform.Management.System.handler merchantId city :<|> API.Action.ProviderPlatform.Management.VehicleInfo.handler merchantId city :<|> API.Action.ProviderPlatform.Management.Volunteer.handler merchantId city
