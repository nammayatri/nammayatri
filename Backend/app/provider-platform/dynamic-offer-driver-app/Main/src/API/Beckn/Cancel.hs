{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Cancel (API, handler) where

import qualified Beckn.ACL.Cancel as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.Cancel as API
import qualified Beckn.Types.Core.Taxi.API.Cancel as Cancel
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Domain.Action.Beckn.Cancel as DCancel
import Domain.Types.Merchant (Merchant)
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
  Capture "merchantId" (Id Merchant)
    :> SignatureAuth "Authorization"
    :> API.CancelAPI

handler :: FlowServer API
handler = cancel

cancel ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  -- Cancel.CancelReq ->
  ByteString ->
  FlowHandler AckResponse
cancel transporterId subscriber reqBS = withFlowHandlerBecknAPI do
  req <- decodeReq reqBS
  dCancelReq <- case req of
    Right reqV2 -> do
      transactionId <- Utils.getTransactionId reqV2.cancelReqContext
      Utils.withTransactionIdLogTag transactionId $ do
        logTagInfo "Cancel APIV2 Flow" "Reached"
        ACL.buildCancelReqV2 reqV2
    Left reqV1 ->
      withTransactionIdLogTag reqV1 $ do
        logTagInfo "Cancel API Flow" "Reached"
        ACL.buildCancelReq reqV1
  case dCancelReq of
    Left cancelReq -> do
      Redis.whenWithLockRedis (cancelLockKey cancelReq.bookingId.getId) 60 $ do
        (merchant, booking) <- DCancel.validateCancelRequest transporterId subscriber cancelReq
        fork ("cancelBooking:" <> cancelReq.bookingId.getId) $
          DCancel.cancel cancelReq merchant booking
    Right cancelSearchReq -> do
      searchTry <- DCancel.validateCancelSearchRequest transporterId subscriber cancelSearchReq
      fork ("cancelSearch:" <> cancelSearchReq.transactionId) $
        DCancel.cancelSearch transporterId cancelSearchReq searchTry
  return Ack

cancelLockKey :: Text -> Text
cancelLockKey id = "Driver:Cancel:BookingId-" <> id

decodeReq :: MonadFlow m => ByteString -> m (Either Cancel.CancelReq Cancel.CancelReqV2)
decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV1 -> pure $ Left reqV1
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV2 -> pure $ Right reqV2
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS
