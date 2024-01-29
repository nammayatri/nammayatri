{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnSelect (API, handler) where

import qualified Beckn.ACL.OnSelect as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnSelect as OnSelect
import qualified BecknV2.OnDemand.Utils.Common as Utils
import Data.Aeson as A
import Data.Text as T
import Data.Text.Encoding as T
import qualified Domain.Action.Beckn.OnSelect as DOnSelect
import Environment
import EulerHS.Prelude (ByteString)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import Tools.Error (GenericError (InvalidRequest))

type API = OnSelect.OnSelectAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onSelect

onSelect ::
  SignatureAuthResult ->
  -- OnSelect.OnSelectReqV2 ->
  ByteString ->
  FlowHandler AckResponse
onSelect _ reqBS = withFlowHandlerBecknAPI do
  req <- decodeReq reqBS
  (mbDOnSelectReq, messageId) <- case req of
    Right reqV2 -> do
      transactionId <- Utils.getTransactionId reqV2.onSelectReqContext
      mbDOnSelectReq <- Utils.withTransactionIdLogTag transactionId $ ACL.buildOnSelectReqV2 reqV2
      messageId <- Utils.getMessageIdText reqV2.onSelectReqContext
      pure (mbDOnSelectReq, messageId)
    Left reqV1 -> do
      mbDOnSelectReq <- withTransactionIdLogTag reqV1 $ ACL.buildOnSelectReq reqV1
      let messageId = reqV1.context.message_id
      pure (mbDOnSelectReq, messageId)
  whenJust mbDOnSelectReq $ \onSelectReq ->
    Redis.whenWithLockRedis (onSelectLockKey messageId) 60 $ do
      validatedOnSelectReq <- DOnSelect.validateRequest onSelectReq
      fork "on select processing" $ do
        Redis.whenWithLockRedis (onSelectProcessingLockKey messageId) 60 $
          DOnSelect.onSelect validatedOnSelectReq
  pure Ack

onSelectLockKey :: Text -> Text
onSelectLockKey id = "Customer:OnSelect:MessageId-" <> id

onSelectProcessingLockKey :: Text -> Text
onSelectProcessingLockKey id = "Customer:OnSelect:Processing:MessageId-" <> id

decodeReq :: MonadFlow m => ByteString -> m (Either OnSelect.OnSelectReq OnSelect.OnSelectReqV2)
decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV1 -> pure $ Left reqV1
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV2 -> pure $ Right reqV2
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS
