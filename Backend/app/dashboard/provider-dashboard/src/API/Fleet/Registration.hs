{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Fleet.Registration
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Dashboard.Fleet.Registration as DReg
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Fleet.Registration as DP
import "lib-dashboard" Domain.Action.Dashboard.Registration as DR
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant hiding (throwError)
import "lib-dashboard" Storage.Queries.Merchant as QMerchant
import "lib-dashboard" Storage.Queries.Person as QP

type API =
  "fleet"
    :> ( FleetOwnerLoginAPI
           :<|> FleetOwnerVerifyAPI
       )

type FleetOwnerLoginAPI = DReg.FleetOwnerLoginAPI

type FleetOwnerVerifyAPI =
  "verify"
    :> "otp"
    :> ReqBody '[JSON] DP.FleetOwnerLoginReq
    :> Post '[JSON] DP.FleetOwnerVerifyRes

handler :: FlowServer API
handler =
  fleetOwnerLogin
    :<|> fleetOwnerVerfiy

fleetOwnerLogin :: DP.FleetOwnerLoginReq -> FlowHandler APISuccess
fleetOwnerLogin req = withFlowHandlerAPI $ do
  Client.callDynamicOfferDriverAppFleetApi (.fleetOwnerLogin) req

fleetOwnerVerfiy :: DP.FleetOwnerLoginReq -> FlowHandler DP.FleetOwnerVerifyRes
fleetOwnerVerfiy req = withFlowHandlerAPI $ do
  person <- QP.findByMobileNumber req.mobileNumber req.mobileCountryCode >>= fromMaybeM (PersonDoesNotExist req.mobileNumber)
  let merchantId = ShortId req.merchantId :: ShortId DM.Merchant
  merchant <- QMerchant.findByShortId merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getShortId)
  _ <- Client.callDynamicOfferDriverAppFleetApi (.fleetOwnerVerify) req
  token <- DR.generateToken person.id merchant.id
  pure $ DP.FleetOwnerVerifyRes {authToken = token}
