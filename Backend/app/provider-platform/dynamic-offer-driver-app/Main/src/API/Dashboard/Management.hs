{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Management where

import qualified API.Action.Dashboard.Management.Booking as BookingDSL
import qualified API.Action.Dashboard.Management.Driver as DriverDSL
import qualified API.Action.Dashboard.Management.DriverCoins as DriverCoinsDSL
import qualified API.Action.Dashboard.Management.DriverGoHome as DriverGoHomeDSL
import qualified API.Action.Dashboard.Management.DriverReferral as DriverReferralDSL
import qualified API.Action.Dashboard.Management.DriverRegistration as DriverRegistrationDSL
import qualified API.Action.Dashboard.Management.Merchant as MerchantDSL
import qualified API.Action.Dashboard.Management.Message as MessageDSL
import qualified API.Action.Dashboard.Management.NammaTag as NammaTagDSL
import qualified API.Action.Dashboard.Management.Revenue as RevenueDSL
import qualified API.Action.Dashboard.Management.Ride as RideDSL
import qualified API.Dashboard.Management.Issue as Issue
import qualified API.Dashboard.Management.Overlay as Overlay
import qualified API.Dashboard.Management.Subscription as Subscription
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Servant hiding (throwError)
import Tools.Auth

type API =
  DashboardTokenAuth
    :> ( Subscription.API
           :<|> Overlay.API
           :<|> Issue.API
           :<|> MerchantDSL.API
           :<|> MessageDSL.API
           :<|> RevenueDSL.API
           :<|> RideDSL.API
           :<|> NammaTagDSL.API
           :<|> DriverDSL.API
           :<|> DriverCoinsDSL.API
           :<|> DriverGoHomeDSL.API
           :<|> DriverReferralDSL.API
           :<|> DriverRegistrationDSL.API
           :<|> BookingDSL.API
       )

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city _ =
  Subscription.handler merchantId city
    :<|> Overlay.handler merchantId city
    :<|> Issue.handler merchantId city
    :<|> MerchantDSL.handler merchantId city
    :<|> MessageDSL.handler merchantId city
    :<|> RevenueDSL.handler merchantId city
    :<|> RideDSL.handler merchantId city
    :<|> NammaTagDSL.handler merchantId city
    :<|> DriverDSL.handler merchantId city
    :<|> DriverCoinsDSL.handler merchantId city
    :<|> DriverGoHomeDSL.handler merchantId city
    :<|> DriverReferralDSL.handler merchantId city
    :<|> DriverRegistrationDSL.handler merchantId city
    :<|> BookingDSL.handler merchantId city
