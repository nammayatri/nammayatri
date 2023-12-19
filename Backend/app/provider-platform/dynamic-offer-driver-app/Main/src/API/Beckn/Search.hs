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
import Storage.Beam.SystemConfigs ()
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
  dSearchReq <-
    case req of
      Right reqV2 ->
        withTransactionIdLogTag reqV2 $ do
          logTagInfo "SearchV2 API Flow" "Reached"
          ACL.buildSearchReqV2 transporterId subscriber reqV2
      Left reqV1 ->
        withTransactionIdLogTag reqV1 $ do
          logTagInfo "Search API Flow" "Reached"
          ACL.buildSearchReqV1 transporterId subscriber reqV1

  Redis.whenWithLockRedis (searchLockKey dSearchReq.messageId transporterId.getId) 60 $ do
    merchant <- DSearch.validateRequest transporterId dSearchReq
    fork "search request processing" $
      Redis.whenWithLockRedis (searchProcessingLockKey dSearchReq.messageId transporterId.getId) 60 $ do
        dSearchRes <- DSearch.handler merchant dSearchReq
        let callbackUrl = gateway.subscriber_url
        (bapUri, bapId, msgId, city, country, txnId, bppId, bppUri) <- case req of
          Left reqV1 -> do
            pure (reqV1.context.bap_uri, reqV1.context.bap_id, reqV1.context.message_id, reqV1.context.city, reqV1.context.country, reqV1.context.transaction_id, reqV1.context.bpp_id, reqV1.context.bpp_uri)
          Right reqV2 ->
            pure (reqV2.context.bap_uri, reqV2.context.bap_id, reqV2.context.message_id, reqV2.context.location.city.code, reqV2.context.location.country.code, reqV2.context.transaction_id, reqV2.context.bpp_id, reqV2.context.bpp_uri)
        internalEndPointHashMap <- asks (.internalEndPointHashMap)
        isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
        if isBecknSpecVersion2
          then do
            context <- buildTaxiContextV2 Context.ON_SELECT msgId txnId bapId bapUri bppId bppUri city country
            logTagInfo "SearchV2 API Flow" "Sending OnSearch"
            void $
              CallBAP.withCallbackV2 dSearchRes.provider Context.SEARCH OnSearch.onSearchAPIV2 context callbackUrl internalEndPointHashMap $ do
                pure $ ACL.mkOnSearchMessageV2 dSearchRes
          else do
            context <- buildTaxiContext Context.ON_SELECT msgId txnId bapId bapUri bppId bppUri city country False
            logTagInfo "Search API Flow" "Sending OnSearch"
            void $
              CallBAP.withCallback dSearchRes.provider Context.SEARCH OnSearch.onSearchAPIV1 context callbackUrl internalEndPointHashMap $ do
                pure $ ACL.mkOnSearchMessage dSearchRes
  pure Ack

searchLockKey :: Text -> Text -> Text
searchLockKey id mId = "Driver:Search:MessageId-" <> id <> ":" <> mId

searchProcessingLockKey :: Text -> Text -> Text
searchProcessingLockKey id mId = "Driver:Search:Processing:MessageId-" <> id <> ":" <> mId

decodeReq :: MonadFlow m => ByteString -> m (Either Search.SearchReq Search.SearchReqV2)
decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV2 -> pure $ Right reqV2
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV1 -> pure $ Left reqV1
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS
