module API.Beckn.FRFS.OnSelect where

import qualified Beckn.ACL.FRFS.OnSelect as ACL
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Data.Aeson (eitherDecodeStrict')
import qualified Domain.Action.Beckn.FRFS.OnSelect as DOnSelect
import Environment
import EulerHS.Prelude (ByteString)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import TransactionLogs.PushLogs

type API = Spec.OnSelectAPIBS

handler :: SignatureAuthResult -> FlowServer API
handler = onSelect

onSelect :: SignatureAuthResult -> ByteString -> FlowHandler Spec.AckResponse
onSelect _ reqBS = withFlowHandlerAPI $ do
  req <- case decodeOnSelectReq reqBS of
    Right r -> pure r
    Left err -> throwError (InvalidRequest (toText err))
  transaction_id <- req.onSelectReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  logDebug $ "Received OnSelect request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    onSelectReq <- ACL.buildOnSelectReq req
    Redis.whenWithLockRedis (onSelectLockKey onSelectReq.messageId) 60 $ do
      (merchant, quote, integratedBppConfig) <- DOnSelect.validateRequest onSelectReq
      fork "FRFS on_select processing" $ do
        Redis.whenWithLockRedis (onSelectProcessingLockKey onSelectReq.messageId) 60 $
          DOnSelect.onSelect onSelectReq merchant quote Nothing Nothing Nothing integratedBppConfig
      fork "FRFS onSelect received pushing ondc logs" do
        void $ pushLogs "on_select" (toJSON req) merchant.id.getId "PUBLIC_TRANSPORT"
  pure Utils.ack

onSelectLockKey :: Text -> Text
onSelectLockKey id = "FRFS:OnSelect:MessageId-" <> id

onSelectProcessingLockKey :: Text -> Text
onSelectProcessingLockKey id = "FRFS:OnSelect:Processing:MessageId-" <> id

decodeOnSelectReq :: ByteString -> Either String Spec.OnSelectReq
decodeOnSelectReq bs =
  case eitherDecodeStrict' bs of
    Right v -> Right v
    Left _ ->
      case Utils.unescapeQuotedJSON bs of
        Just inner ->
          eitherDecodeStrict' inner
        Nothing ->
          Left "Unable to decode OnSelectReq: invalid JSON format."
