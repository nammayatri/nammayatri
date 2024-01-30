{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.FRFS.OnCancel where

import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Environment
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()

type API = Spec.OnCancelAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onCancel

onCancel ::
  SignatureAuthResult ->
  Spec.OnCancelReq ->
  FlowHandler Spec.AckResponse
onCancel _ req = withFlowHandlerAPI $ do
  logDebug $ "Received OnCancel request" <> encodeToText req
  -- TODO add onCancel handler once requirement is added
  pure Utils.ack
