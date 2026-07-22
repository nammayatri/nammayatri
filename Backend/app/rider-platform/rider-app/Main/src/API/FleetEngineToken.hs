{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.FleetEngineToken where

import qualified Domain.Action.UI.FleetEngineToken as DFleetEngineToken
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import Environment
import Kernel.Prelude
import Kernel.Types.App (MandatoryQueryParam)
import Kernel.Types.Id
import Servant
import Tools.Auth (TokenAuth)
import Tools.FlowHandling (withFlowHandlerAPIPersonId)

type API =
  TokenAuth
    :> "fleetEngine"
    :> "consumerToken"
    :> MandatoryQueryParam "rideId" (Id Ride.Ride)
    :> Get '[JSON] DFleetEngineToken.FleetEngineConsumerTokenRes

handler :: FlowServer API
handler = getFleetEngineConsumerToken

getFleetEngineConsumerToken ::
  (Id Person.Person, Id Merchant.Merchant) ->
  Id Ride.Ride ->
  FlowHandler DFleetEngineToken.FleetEngineConsumerTokenRes
getFleetEngineConsumerToken (personId, merchantId) rideId =
  withFlowHandlerAPIPersonId personId $
    DFleetEngineToken.getFleetEngineConsumerToken (personId, merchantId) rideId
