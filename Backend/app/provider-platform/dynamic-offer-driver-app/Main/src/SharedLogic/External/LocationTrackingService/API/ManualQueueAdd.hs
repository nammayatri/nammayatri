{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.External.LocationTrackingService.API.ManualQueueAdd where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified EulerHS.Types as ET
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Servant

data ManualQueueAddRequest = ManualQueueAddRequest
  { queuePosition :: Int
  }
  deriving (Generic, ToJSON, FromJSON, Show)

type ManualQueueAddAPI =
  "internal"
    :> "special-locations"
    :> Capture "specialLocationId" Text
    :> "queue"
    :> Capture "vehicleType" Text
    :> "drivers"
    :> Capture "merchantId" (Id DM.Merchant)
    :> Capture "driverId" (Id DP.Person)
    :> ReqBody '[JSON] ManualQueueAddRequest
    :> Post '[JSON] APISuccess

manualQueueAddAPI :: Proxy ManualQueueAddAPI
manualQueueAddAPI = Proxy

manualQueueAdd :: Text -> Text -> Id DM.Merchant -> Id DP.Person -> ManualQueueAddRequest -> ET.EulerClient APISuccess
manualQueueAdd = ET.client manualQueueAddAPI
