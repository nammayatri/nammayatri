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

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FleetEngine as FleetEngine

-- | A short-lived, vehicle-scoped Fleet Engine driver JWT for the driver app's
-- Driver SDK, plus the @vehicleId@ it is scoped to and the Fleet Engine
-- @providerId@ the SDK must be initialised with.
data FleetEngineDriverTokenRes = FleetEngineDriverTokenRes
  { token :: Text,
    vehicleId :: Text,
    providerId :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

getFleetEngineDriverToken ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  (Id Person.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) ->
  m FleetEngineDriverTokenRes
getFleetEngineDriverToken (personId, _, merchantOpCityId) = do
  (token, vehicleId, providerId) <-
    FleetEngine.mkDriverToken merchantOpCityId personId
      >>= fromMaybeM (InternalError "Fleet Engine is not configured for this city")
  pure FleetEngineDriverTokenRes {..}
