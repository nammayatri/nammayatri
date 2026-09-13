{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.BulkLogicRollout
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.NammaTag
import qualified Domain.Action.RiderPlatform.Management.NammaTag as Domain
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import qualified Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI')
import qualified Lib.Yudhishthira.Types
import Servant
import Tools.Auth.Api

type API =
  "nammaTag" :> "appDynamicLogic" :> "bulkUpsertLogicRollout"
    :> ApiAuth
         ('DRIVER_OFFER_BPP_MANAGEMENT)
         ('DSL)
         (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.NAMMA_TAG) / ('API.Types.ProviderPlatform.Management.NammaTag.APP_DYNAMIC_LOGIC_ROLLOUT))
    :> ReqBody '[JSON] Lib.Yudhishthira.Types.BulkLogicRolloutReq
    :> Post '[JSON] Lib.Yudhishthira.Types.BulkLogicRolloutResult

handler :: Kernel.Types.Id.ShortId DM.Merchant -> FlowServer API
handler merchantId apiTokenInfo req =
  withFlowHandlerAPI' $
    Domain.postNammaTagAppDynamicLogicBulkUpsertLogicRollout merchantId apiTokenInfo req
