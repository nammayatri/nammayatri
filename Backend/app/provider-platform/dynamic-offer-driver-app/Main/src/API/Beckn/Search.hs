{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Search (API, handler) where

import qualified Beckn.ACL.OnSearch as ACL
import qualified Beckn.ACL.Search as ACL
import qualified Beckn.Core as CallBAP
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import Tools.Error (GenericError (InvalidRequest))

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> SignatureAuth "X-Gateway-Authorization"
    :> Search.SearchAPI

handler :: FlowServer API
handler = search

search ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  SignatureAuthResult ->
  -- Search.SearchReq ->
  ByteString ->
  FlowHandler AckResponse
search transporterId (SignatureAuthResult _ subscriber) (SignatureAuthResult _ gateway) reqBS = withFlowHandlerBecknAPI $ do
  req <- decodeReq reqBS
  case req of
    Left reqV1 ->
      withTransactionIdLogTag reqV1 $ do
        logTagInfo "Search API Flow" "Reached"
        dSearchReq <- ACL.buildSearchReqV1 transporterId subscriber reqV1
        Redis.whenWithLockRedis (searchLockKey dSearchReq.messageId transporterId.getId) 60 $ do
          merchant <- DSearch.validateRequest transporterId dSearchReq
          fork "search request processing" $
            Redis.whenWithLockRedis (searchProcessingLockKey dSearchReq.messageId transporterId.getId) 60 $ do
              dSearchRes <- DSearch.handler merchant dSearchReq
              let context = reqV1.context
              let callbackUrl = gateway.subscriber_url
              void $
                CallBAP.withCallback dSearchRes.provider Context.SEARCH OnSearch.onSearchAPI context callbackUrl $ do
                  pure $ ACL.mkOnSearchMessage dSearchRes
        pure Ack
    Right reqV2 ->
      withTransactionIdLogTag reqV2 $ do
        logTagInfo "Search API Flow" "Reached"
        dSearchReq <- ACL.buildSearchReqV2 transporterId subscriber reqV2
        Redis.whenWithLockRedis (searchLockKey dSearchReq.messageId transporterId.getId) 60 $ do
          merchant <- DSearch.validateRequest transporterId dSearchReq
          fork "search request processing" $
            Redis.whenWithLockRedis (searchProcessingLockKey dSearchReq.messageId transporterId.getId) 60 $ do
              dSearchRes <- DSearch.handler merchant dSearchReq
              let context = reqV2.context
              let callbackUrl = gateway.subscriber_url
              void $
                CallBAP.withCallback dSearchRes.provider Context.SEARCH OnSearch.onSearchAPIV2 context callbackUrl $ do
                  pure $ ACL.mkOnSearchMessageV2 dSearchRes
        pure Ack

searchLockKey :: Text -> Text -> Text
searchLockKey id mId = "Driver:Search:MessageId-" <> id <> ":" <> mId

searchProcessingLockKey :: Text -> Text -> Text
searchProcessingLockKey id mId = "Driver:Search:Processing:MessageId-" <> id <> ":" <> mId

decodeReq :: MonadFlow m => ByteString -> m (Either Search.SearchReq Search.SearchReqV2)
decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV1 -> pure $ Left reqV1
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV2 -> pure $ Right reqV2
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS
