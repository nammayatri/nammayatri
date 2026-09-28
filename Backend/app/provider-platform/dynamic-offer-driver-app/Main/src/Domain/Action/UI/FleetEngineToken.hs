{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.FleetEngineToken
  ( FleetEngineDriverTokenRes (..),
    getFleetEngineDriverToken,
  )
where

import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FleetEngine as FleetEngine

-- | Driver JWT + vehicleId + providerId — all three needed to initialise the Driver SDK on the phone.
-- All three are Nothing when Fleet Engine is off for the city (config missing or kill-switch off);
-- the driver app skips SDK init in that case without treating it as an error.
data FleetEngineDriverTokenRes = FleetEngineDriverTokenRes
  { token :: Maybe Text,
    vehicleId :: Maybe Text,
    providerId :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

getFleetEngineDriverToken ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    HasKafkaProducer r
  ) =>
  (Id Person.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) ->
  m FleetEngineDriverTokenRes
getFleetEngineDriverToken (personId, _, merchantOpCityId) = do
  mbTok <- FleetEngine.mkDriverToken merchantOpCityId personId
  pure $ case mbTok of
    Nothing -> FleetEngineDriverTokenRes {token = Nothing, vehicleId = Nothing, providerId = Nothing}
    Just (t, v, p) -> FleetEngineDriverTokenRes {token = Just t, vehicleId = Just v, providerId = Just p}
