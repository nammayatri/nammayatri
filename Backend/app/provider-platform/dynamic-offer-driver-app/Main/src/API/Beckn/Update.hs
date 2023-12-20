{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Update (API, handler) where

import qualified Beckn.ACL.Update as ACL
import qualified Beckn.Types.Core.Taxi.API.Update as Update
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Domain.Action.Beckn.Update as DUpdate
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import Tools.Error (GenericError (InvalidRequest))
import Storage.Beam.SystemConfigs ()

type API =
  Capture "merchantId" (Id Merchant)
    :> SignatureAuth "Authorization"
    :> Update.UpdateAPI

handler :: FlowServer API
handler = update

update ::
  Id Merchant ->
  SignatureAuthResult ->
  -- Update.UpdateReq ->
  ByteString ->
  FlowHandler AckResponse
update _ (SignatureAuthResult _ subscriber) reqBS = withFlowHandlerBecknAPI $ do
  req <- decodeReq reqBS
  dUpdateReq <- case req of
    Right reqV2 ->
      withTransactionIdLogTag reqV2 $ do
        logTagInfo "updateV2 API Flow" "Reached"
        ACL.buildUpdateReqV2 subscriber reqV2
    Left reqV1 ->
      withTransactionIdLogTag reqV1 $ do
        logTagInfo "update API Flow" "Reached"
        ACL.buildUpdateReqV1 subscriber reqV1

  Redis.whenWithLockRedis (updateLockKey dUpdateReq.bookingId.getId) 60 $ do
    fork "update request processing" $
      Redis.whenWithLockRedis (updateProcessingLockKey dUpdateReq.bookingId.getId) 60 $
        DUpdate.handler dUpdateReq
  pure Ack

updateLockKey :: Text -> Text
updateLockKey id = "Driver:Update:BookingId-" <> id

updateProcessingLockKey :: Text -> Text
updateProcessingLockKey id = "Driver:Update:Processing:BookingId-" <> id

decodeReq :: MonadFlow m => ByteString -> m (Either Update.UpdateReq Update.UpdateReqV2)
decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV2 -> pure $ Right reqV2
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV1 -> pure $ Left reqV1
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS
