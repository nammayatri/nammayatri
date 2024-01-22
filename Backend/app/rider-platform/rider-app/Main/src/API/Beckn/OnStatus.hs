{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnStatus (API, handler) where

import qualified Beckn.ACL.OnStatus as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnStatus as OnStatus
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Domain.Action.Beckn.OnStatus as DOnStatus
import Environment
import EulerHS.Prelude (ByteString)
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import Tools.Error

type API = OnStatus.OnStatusAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onStatus

onStatus ::
  SignatureAuthResult ->
  -- OnStatus.OnStatusReq ->
  ByteString ->
  FlowHandler AckResponse
onStatus _ reqBS = withFlowHandlerBecknAPI do
  req <- decodeReq reqBS
  mbDOnStatusReq <- case req of
    Right reqV2 -> do
      transactionId <- Utils.getTransactionId reqV2.onStatusReqContext
      Utils.withTransactionIdLogTag transactionId $ ACL.buildOnStatusReqV2 reqV2
    Left reqV1 ->
      withTransactionIdLogTag reqV1 $
        ACL.buildOnStatusReq reqV1
  whenJust mbDOnStatusReq DOnStatus.onStatus
  pure Ack

decodeReq :: MonadFlow m => ByteString -> m (Either OnStatus.OnStatusReq OnStatus.OnStatusReqV2)
decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV2 -> pure $ Right reqV2
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV1 -> pure $ Left reqV1
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS
