{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnConfirm (API, handler) where

import qualified Beckn.ACL.OnConfirm as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import Data.Aeson as A
import Data.Text as T
import Data.Text.Encoding as T
import qualified Domain.Action.Beckn.OnConfirm as DOnConfirm
import Environment
import EulerHS.Prelude (ByteString)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import Tools.Error (GenericError (InvalidRequest))

type API = OnConfirm.OnConfirmAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onConfirm

onConfirm ::
  SignatureAuthResult ->
  -- OnConfirm.OnConfirmReqV2 ->
  ByteString ->
  FlowHandler AckResponse
onConfirm _ reqBS = withFlowHandlerBecknAPI do
  req <- decodeReq reqBS
  mbDOnConfirmReq <- case req of
    Right reqV2 -> do
      transactionId <- Utils.getTransactionId reqV2.onConfirmReqContext
      Utils.withTransactionIdLogTag transactionId $ ACL.buildOnConfirmReqV2 reqV2
    Left reqV1 -> withTransactionIdLogTag reqV1 $ ACL.buildOnConfirmReq reqV1
  whenJust mbDOnConfirmReq $ \onConfirmReq ->
    Redis.whenWithLockRedis (onConfirmLockKey onConfirmReq.bppBookingId.getId) 60 $ do
      validatedReq <- DOnConfirm.validateRequest onConfirmReq
      fork "onConfirm request processing" $
        Redis.whenWithLockRedis (onConfirmProcessingLockKey onConfirmReq.bppBookingId.getId) 60 $
          DOnConfirm.onConfirm validatedReq
  pure Ack

onConfirmLockKey :: Text -> Text
onConfirmLockKey id = "Customer:OnConfirm:BppBookingId-" <> id

onConfirmProcessingLockKey :: Text -> Text
onConfirmProcessingLockKey id = "Customer:OnConfirm:Processing:BppBookingId-" <> id

decodeReq :: MonadFlow m => ByteString -> m (Either OnConfirm.OnConfirmReq OnConfirm.OnConfirmReqV2)
decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV2 -> pure $ Right reqV2
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV1 -> pure $ Left reqV1
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS
