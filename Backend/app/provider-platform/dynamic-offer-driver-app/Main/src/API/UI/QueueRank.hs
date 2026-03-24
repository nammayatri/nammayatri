{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.QueueRank
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.QueueRank as DQueueRank
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "queue"
    :> Capture "specialLocationId" Text
    :> Capture "vehicleType" Text
    :> "position"
    :> TokenAuth
    :> Get '[JSON] DQueueRank.QueueRankResponse

handler :: FlowServer API
handler = getQueuePosition

getQueuePosition :: Text -> Text -> (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DQueueRank.QueueRankResponse
getQueuePosition specialLocationId vehicleType (driverId, merchantId, merchantOpCityId) =
  withFlowHandlerAPI $ DQueueRank.getQueuePosition (driverId, merchantId, merchantOpCityId) specialLocationId vehicleType
