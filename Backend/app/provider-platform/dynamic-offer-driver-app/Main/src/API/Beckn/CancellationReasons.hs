{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.CancellationReasons (API, handler) where -- should move to SharedLogic

import qualified Beckn.Types.Core.Taxi.API.CancellationReasons as API
import qualified Beckn.Types.Core.Taxi.CancellationReasons.Types as API
import qualified Domain.Action.UI.CancellationReason as Q
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> SignatureAuth "Authorization"
    :> API.CancellationReasonsAPI

handler :: FlowServer API
handler = sendCancellationReasons

sendCancellationReasons ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  FlowHandler API.CancellationReasons
sendCancellationReasons _ _ =
  withFlowHandlerAPI $ do
    Q.getCancellationReasons
