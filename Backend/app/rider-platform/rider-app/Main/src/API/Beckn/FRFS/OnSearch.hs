{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.FRFS.OnSearch where

import qualified Beckn.ACL.FRFS.OnSearch as ACL
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Data.Aeson (eitherDecodeStrict')
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Domain.Action.Beckn.FRFS.OnSearch as DOnSearch
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Environment
import EulerHS.Prelude (ByteString)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import TransactionLogs.PushLogs

type API = Spec.OnSearchAPIBS

handler :: SignatureAuthResult -> FlowServer API
handler = onSearch

onSearch :: SignatureAuthResult -> ByteString -> FlowHandler Spec.AckResponse
onSearch _ reqBS = withFlowHandlerAPI $ do
  req <- case decodeOnSearchReq reqBS of
    Right r -> pure r
    Left err -> throwError (InvalidRequest (toText err))
  transaction_id <- req.onSearchReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  withTransactionIdLogTag' transaction_id $ do
    message_id <- req.onSearchReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
    logDebug $ "Received OnSearch request" <> encodeToText req
    mbDiscoveryCounter :: Maybe DOnSearch.DiscoveryCounter <- Redis.safeGet (Utils.discoverySearchCounterKey (show transaction_id))
    case mbDiscoveryCounter of
      Just discoveryCounter -> do
        logDebug $ "Using discovery on_search logic for txn: " <> show transaction_id
        Redis.whenWithLockRedis (onSearchLockKey message_id) 60 $ do
          onSearchReq <- ACL.buildDiscoveryOnSearchReq req discoveryCounter
          fork "FRFS discovery on_search processing" $ do
            Redis.whenWithLockRedis (onSearchProcessingLockKey message_id) 60 $
              DOnSearch.discoveryOnSearch onSearchReq
          fork "FRFS discovery onSearch logs" $ do
            void $ pushLogs "discovery_on_search" (toJSON req) onSearchReq.merchantId "PUBLIC_TRANSPORT"
        pure Utils.ack
      Nothing -> do
        logDebug $ "Using normal on_search logic for txn: " <> show transaction_id
        Redis.whenWithLockRedis (onSearchLockKey message_id) 60 $ do
          onSearchReq <- ACL.buildOnSearchReq req
          validatedReq <- DOnSearch.validateRequest onSearchReq

          let fareCachingAllowed = case validatedReq.integratedBppConfig.providerConfig of
                DIBC.ONDC ondcCfg -> fromMaybe False (DIBC.fareCachingAllowed ondcCfg)
                _ -> False

          if fareCachingAllowed
            then do
              fork "FRFS on_search fare cache refresh" $ do
                Redis.whenWithLockRedis (onSearchProcessingLockKey message_id) 60 $
                  DOnSearch.upsertFareCache onSearchReq validatedReq
            else do
              fork "FRFS normal on_search processing" $ do
                Redis.whenWithLockRedis (onSearchProcessingLockKey message_id) 60 $
                  DOnSearch.onSearch onSearchReq validatedReq
          fork "FRFS onSearch logs" $ do
            void $ pushLogs "on_search" (toJSON req) validatedReq.merchant.id.getId "PUBLIC_TRANSPORT"
        pure Utils.ack

onSearchLockKey :: Text -> Text
onSearchLockKey id = "FRFS:OnSearch:MessageId-" <> id

onSearchProcessingLockKey :: Text -> Text
onSearchProcessingLockKey id = "FRFS:OnSearch:Processing:MessageId-" <> id

decodeOnSearchReq :: ByteString -> Either String Spec.OnSearchReq
decodeOnSearchReq bs =
  case eitherDecodeStrict' bs of
    Right v -> Right v
    Left _ ->
      case Utils.unescapeQuotedJSON bs of
        Just inner ->
          eitherDecodeStrict' inner
        Nothing ->
          Left "Unable to decode OnSearchReq: invalid JSON format."

unescapeQuotedJSON :: ByteString -> Maybe ByteString
unescapeQuotedJSON bs =
  if BS.length bs >= 2 && BS.head bs == 34 && BS.last bs == 34
    then
      let inner = BS.init (BS.tail bs)
       in case eitherDecodeStrict' ("\"" <> inner <> "\"") :: Either String T.Text of
            Right txt -> Just (TE.encodeUtf8 txt)
            Left _ -> Nothing
    else Nothing
