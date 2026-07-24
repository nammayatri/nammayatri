{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.PersonCreate
  ( API,
    handler,
  )
where

import qualified API.Types.UnifiedDashboard.Management.Person as UDPerson
import qualified Domain.Action.UnifiedDashboard.Management.Person as Domain
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id (ShortId (..))
import Kernel.Utils.Common
import Servant hiding (throwError)
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.SystemConfigs ()

-- | Thin internal (api-key authed) wrapper that reuses the existing
-- UnifiedDashboard person-create handler so dashboards can create a driver-side
-- person entry for BPP-synced roles.
type API =
  "person"
    :> "create"
    :> Capture "merchantShortId" Text
    :> Capture "city" Context.City
    :> Header "api-key" Text
    :> ReqBody '[JSON] UDPerson.CreatePersonReq
    :> Post '[JSON] UDPerson.CreatePersonResp

handler :: FlowServer API
handler = personCreate

personCreate :: Text -> Context.City -> Maybe Text -> UDPerson.CreatePersonReq -> FlowHandler UDPerson.CreatePersonResp
personCreate merchantShortId city apiKey req = withFlowHandlerAPI $ do
  merchant <- findMerchantByShortId (ShortId merchantShortId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError (AuthBlocked "Invalid BPP internal api key")
  Domain.postPersonCreate (ShortId merchantShortId) city req
