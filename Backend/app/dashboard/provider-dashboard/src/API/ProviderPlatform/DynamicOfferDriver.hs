{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver
  ( API,
    APIV2,
    InternalAPI,
    handler,
    handlerV2,
    handlerV3,
  )
where

import qualified API.Action.ProviderPlatform.AppManagement as AppManagementDSL
import qualified API.Action.ProviderPlatform.Fleet as FleetDSL
import qualified API.Action.ProviderPlatform.IssueManagement as IssueManagementDSL
import qualified API.Action.ProviderPlatform.Management as ManagementDSL
import qualified API.Action.ProviderPlatform.Operator as OperatorDSL
import qualified API.Action.ProviderPlatform.RideBooking as RideBookingDSL
import qualified API.ProviderPlatform.DynamicOfferDriver.CacAuth as CacAuth
import qualified API.ProviderPlatform.DynamicOfferDriver.InternalAuth as InternalAuth
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Servant

-- TODO: Deprecated, Remove after successful deployment
type API =
  "driver-offer"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> API'

type APIV2 =
  "driver-offer"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> Capture "city" City.City
    :> API'

type InternalAPI =
  "driver-offer"
    :> ( CacAuth.API
           :<|> InternalAuth.API
       )

type API' =
  FleetDSL.API
    :<|> AppManagementDSL.API
    :<|> ManagementDSL.API
    :<|> IssueManagementDSL.API
    :<|> RideBookingDSL.API
    :<|> OperatorDSL.API

-- TODO: Deprecated, Remove after successful deployment
handler :: FlowServer API
handler merchantId = do
  let city = getCity merchantId.getShortId
  FleetDSL.handler merchantId city
    :<|> AppManagementDSL.handler merchantId city
    :<|> ManagementDSL.handler merchantId city
    :<|> IssueManagementDSL.handler merchantId city
    :<|> RideBookingDSL.handler merchantId city
    :<|> OperatorDSL.handler merchantId city
  where
    getCity = \case
      "NAMMA_YATRI_PARTNER" -> City.City "Bangalore"
      "YATRI_PARTNER" -> City.City "Kochi"
      "JATRI_SAATHI_PARTNER" -> City.City "Kolkata"
      _ -> City.City "AnyCity"

handlerV2 :: FlowServer APIV2
handlerV2 merchantId city =
  FleetDSL.handler merchantId city
    :<|> AppManagementDSL.handler merchantId city
    :<|> ManagementDSL.handler merchantId city
    :<|> IssueManagementDSL.handler merchantId city
    :<|> RideBookingDSL.handler merchantId city
    :<|> OperatorDSL.handler merchantId city

handlerV3 :: FlowServer InternalAPI
handlerV3 =
  CacAuth.handler
    :<|> InternalAuth.handler
