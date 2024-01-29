{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Select (API, handler) where

import qualified Beckn.ACL.Select as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.Select as Select
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Domain.Action.Beckn.Select as DSelect
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import Tools.Error (GenericError (InvalidRequest))

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> Select.SelectAPI

handler :: FlowServer API
handler = select

select ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  -- Select.SelectReq ->
  ByteString ->
  FlowHandler AckResponse
select transporterId (SignatureAuthResult _ subscriber) reqBS = withFlowHandlerBecknAPI $ do
  req <- decodeReq reqBS
  dSelectReq <- case req of
    Right reqV2 -> do
      transactionId <- Utils.getTransactionId reqV2.selectReqContext
      Utils.withTransactionIdLogTag transactionId $ do
        logTagInfo "SelectV2 API Flow" "Reached"
        ACL.buildSelectReqV2 subscriber reqV2
    Left reqV1 ->
      withTransactionIdLogTag reqV1 $ do
        logTagInfo "Select API Flow" "Reached"
        ACL.buildSelectReq subscriber reqV1

  Redis.whenWithLockRedis (selectLockKey dSelectReq.messageId) 60 $ do
    (merchant, estimate) <- DSelect.validateRequest transporterId dSelectReq
    fork "select request processing" $ do
      Redis.whenWithLockRedis (selectProcessingLockKey dSelectReq.messageId) 60 $
        DSelect.handler merchant dSelectReq estimate
  pure Ack

selectLockKey :: Text -> Text
selectLockKey id = "Driver:Select:MessageId-" <> id

selectProcessingLockKey :: Text -> Text
selectProcessingLockKey id = "Driver:Select:Processing:MessageId-" <> id

decodeReq :: MonadFlow m => ByteString -> m (Either Select.SelectReq Select.SelectReqV2)
decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV1 -> pure $ Left reqV1
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV2 -> pure $ Right reqV2
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS
