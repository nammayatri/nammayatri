{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.Estimate (API, handler) where

import Domain.Action.Internal.Estimate as Estimate
import Domain.Types.Estimate as Estimate
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Servant

type API =
  "estimates"
    :> Capture "estimateId" (Id Estimate)
    :> Header "token" Text
    :> Get '[JSON] Estimate.BppEstimate

handler :: FlowServer API
handler = Estimate.getEstimateDetails
