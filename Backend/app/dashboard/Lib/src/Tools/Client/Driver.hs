{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Client.Driver
  ( callDriverUpdatePersonMobileByFleetRole,
    DriverAPIs (..),
    mkDriverAPIs,
  )
where

import qualified "dynamic-offer-driver-app" API.Dashboard.Person as BPPPerson
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.ServerName as DTServer
import qualified EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Utils.Common
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client (CallServerAPI, callServerAPI, clientWithMerchantAndCity)
import Tools.Metrics

newtype DriverAPIs = DriverAPIs
  { updatePersonMobileByFleetRole :: BPPPerson.UpdatePersonMobileByFleetRoleReq -> Euler.EulerClient APISuccess
  }

mkDriverAPIs :: CheckedShortId DMerchant.Merchant -> City.City -> Text -> DriverAPIs
mkDriverAPIs merchantId city _token = do
  let updatePersonMobileByFleetRole = personClient merchantId city
  DriverAPIs {..}
  where
    personClient = clientWithMerchantAndCity (Proxy :: Proxy BPPPerson.API)

callDriverUpdatePersonMobileByFleetRole ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    CallServerAPI DriverAPIs m r b c
  ) =>
  CheckedShortId DMerchant.Merchant ->
  City.City ->
  (DriverAPIs -> b) ->
  c
callDriverUpdatePersonMobileByFleetRole checkedMerchantId city = do
  callServerAPI @_ @m @r DTServer.DRIVER_OFFER_BPP_MANAGEMENT (mkDriverAPIs checkedMerchantId city) "callDriverUpdatePersonMobileByFleetRole"
