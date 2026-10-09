{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.BetterDriverSearch
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.BetterDriverSearch as DBetterDriverSearch
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (withPersonIdLogTag)
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth
import Tools.FlowHandling (withFlowHandlerAPIPersonId)

type API = BetterDriverSearchAPI

type BetterDriverSearchAPI =
  "rideBooking"
    :> Capture "rideBookingId" (Id SRB.Booking)
    :> "betterDriverSearch"
    :> TokenAuth
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = betterDriverSearch

betterDriverSearch ::
  Id SRB.Booking ->
  (Id Person.Person, Id Merchant.Merchant) ->
  FlowHandler APISuccess
betterDriverSearch bookingId (personId, _merchantId) =
  withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $
    DBetterDriverSearch.postRideBookingBetterDriverSearch personId bookingId
