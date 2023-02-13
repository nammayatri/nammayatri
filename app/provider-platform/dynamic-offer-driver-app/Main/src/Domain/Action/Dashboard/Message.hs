module Domain.Action.Dashboard.Message where 

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Message as Common
import qualified Domain.Types.Merchant as DM
import qualified Data.ByteString as BS
import Data.Csv
import qualified Data.Vector as V
import qualified Storage.Queries.Message.MessageReport as MRQuery 
import qualified Storage.Queries.Message.Message as MQuery 
import qualified Storage.Queries.Message.MediaFile as MFQuery 
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Domain.Types.Message.MessageReport as Domain 
import qualified Domain.Types.Message.Message as Domain 
import qualified Domain.Types.Message.MediaFile as Domain 
import Kernel.Prelude
import SharedLogic.Transporter (findMerchantByShortId)
import Kernel.Types.Id
import Environment
import qualified Kernel.External.FCM.Types as FCM
import qualified EulerHS.Language as L
import Data.Map as M
import qualified Storage.Queries.Person as Person
import qualified AWS.S3 as S3
import qualified Data.Text as T
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Common (MonadTime (getCurrentTime), GuidLike (generateGUID), Forkable (fork))
import Data.Time.Format.ISO8601 (iso8601Show)
import EulerHS.Types (base64Encode)
import Kernel.Utils.Common (throwError, fromMaybeM)
import Kernel.Types.Error (GenericError(InvalidRequest))
import qualified Data.ByteString.Lazy as LBS
import Tools.Notifications (sendNotificationToDriver)
import Control.Monad.Extra (mapMaybeM)

createFilePath ::
  (MonadTime m, MonadReader r m, HasField "s3Env" r (S3.S3Env m)) =>
  Text ->
  Common.FileType ->
  m Text
createFilePath merchantId fileType = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return
    ( pathPrefix <> "/message-media/" <> "org-" <> merchantId <> "/"
        <> show fileType
        <> "/"
        <> fileName
        <> ".zip"
    )

uploadFile :: ShortId DM.Merchant -> Common.UploadFileRequest -> Flow Common.UploadFileResponse
uploadFile merchantShortId Common.UploadFileRequest {..} = do
    merchant <- findMerchantByShortId merchantShortId
    mediaFile <- L.runIO $ base64Encode <$> BS.readFile file
    filePath <- createFilePath merchant.id.getId fileType
    let host = "https://tobedone.com" -- TODO add actual bucket url 
    baseFileUrl <- parseBaseUrl $ host <> filePath
    _ <- fork "S3 put file" $ S3.put (T.unpack filePath) mediaFile
    fileEntity <- mkFile baseFileUrl
    Esq.runTransaction $ MFQuery.create fileEntity
    return $ Common.UploadFileResponse { fileId = cast $ fileEntity.id }
    where 
      mapToDomain = \case
                       Common.Audio -> Domain.Audio
                       Common.Video -> Domain.Video
                       Common.Image -> Domain.Image
      mkFile filePath = do
            id <- generateGUID
            now <- getCurrentTime
            return $ 
                Domain.MediaFile 
                    { id,
                      _type = mapToDomain fileType,
                      url = filePath,
                      createdAt = now
                    }
    
toDomainType  :: Common.MessageType -> Domain.MessageType
toDomainType = \case 
        Common.Read -> Domain.Read 
        Common.Action str -> Domain.Action str
        
toDomainDeliveryStatusType :: Common.MessageDeliveryStatus -> Domain.DeliveryStatus 
toDomainDeliveryStatusType = \case 
    Common.Success -> Domain.Success 
    Common.Pending -> Domain.Pending
    Common.Failed -> Domain.Failed

toCommonDeliveryStatusType :: Domain.DeliveryStatus -> Common.MessageDeliveryStatus 
toCommonDeliveryStatusType = \case 
    Domain.Success -> Common.Success 
    Domain.Pending -> Common.Pending
    Domain.Failed -> Common.Failed


toCommonType ::  Domain.MessageType -> Common.MessageType
toCommonType =  \case 
        Domain.Read -> Common.Read 
        Domain.Action str -> Common.Action str

toCommonMediaFileType :: Domain.MediaType -> Common.FileType
toCommonMediaFileType = \case
                       Domain.Audio -> Common.Audio
                       Domain.Video -> Common.Video
                       Domain.Image -> Common.Image

translationToDomainType :: UTCTime -> Common.MessageTranslation -> Domain.MessageTranslation
translationToDomainType createdAt Common.MessageTranslation {..} = Domain.MessageTranslation {..}

addMessage :: ShortId DM.Merchant -> Common.AddMessageRequest -> Flow Common.AddMessageResponse
addMessage merchantShortId Common.AddMessageRequest {..} = do
    merchant <- findMerchantByShortId merchantShortId
    message <- mkMessage merchant
    Esq.runTransaction $ MQuery.create message
    return $ Common.AddMessageResponse { messageId = cast $ message.id }
    where 
      mkMessage merchant = do
        id <- generateGUID
        now <- getCurrentTime
        return $ 
            Domain.Message 
                { id,
                  merchantId = merchant.id,
                  _type = toDomainType _type,
                  title,
                  description,
                  mediaFiles = cast <$> mediaFiles,
                  messageTranslations = translationToDomainType now <$> translations,
                  createdAt = now
                }

newtype CSVRow = CSVRow { driverId :: String }
instance FromNamedRecord CSVRow where
    parseNamedRecord r = CSVRow <$> r .: "driverId"

sendMessage :: ShortId DM.Merchant -> Common.SendMessageRequest -> Flow APISuccess
sendMessage merchantShortId Common.SendMessageRequest {..} = do
    merchant <- findMerchantByShortId merchantShortId
    message <- Esq.runInReplica $ MQuery.findById (Id messageId) >>= fromMaybeM (InvalidRequest "Message Not Found")
    now <- getCurrentTime
    -- reading of csv file
    csvData <- L.runIO $ BS.readFile csvFile
    driverIds <- 
        case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector CSVRow)) of
            Left err -> throwError . InvalidRequest $ show err
            Right (_, v) -> pure $ V.toList $ V.map (Id . T.pack . (.driverId)) v
    
    Esq.runTransaction $ MRQuery.createMany (mkMessageReport now <$> driverIds)

    mapM_ (sendMessage' message merchant . cast) driverIds
    return Success
    where 
        mkMessageReport now driverId = 
            Domain.MessageReport 
                { driverId
                , messageId = Id messageId
                , deliveryStatus = Domain.Pending 
                , readStatus = False
                , messageDynamicFields = M.empty 
                , reply = Nothing
                , createdAt = now 
                , updatedAt = now
                }
        sendMessage' message merchant driverId = do
            mDriver <- Person.findById driverId 
            case mDriver of 
                Just driver -> do 
                    fork "Sending message to driver" 
                        ( do
                        exep <- try @_ @SomeException (sendNotificationToDriver merchant.id FCM.SHOW Nothing FCM.TRIGGER_SERVICE message.title message.description driver.id driver.deviceToken)
                        case exep of 
                            Left _ -> Esq.runTransaction $ MRQuery.updateDeliveryStatusByMessageIdAndDriverId message.id (cast driverId) Domain.Failed
                            Right _ -> Esq.runTransaction $ MRQuery.updateDeliveryStatusByMessageIdAndDriverId message.id (cast driverId) Domain.Success
                        )
                Nothing -> Esq.runTransaction $ MRQuery.updateDeliveryStatusByMessageIdAndDriverId message.id (cast driverId) Domain.Failed

messageList :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Flow Common.MessageListResponse
messageList merchantShortId mbLimit mbOffset = do
    merchant <- findMerchantByShortId merchantShortId
    messages <- MQuery.findAllWithLimitOffset mbLimit mbOffset merchant.id
    let count = length messages
    let summary = Common.Summary {totalCount = count, count}
    return $ Common.MessageListResponse { messages = buildMessage <$> messages, summary}
    where 
        buildMessage :: Domain.RawMessage -> Common.MessageListItem
        buildMessage message = do 
            Common.MessageListItem
                { messageId = cast message.id 
                , title = message.title 
                , _type = toCommonType message._type
                }

messageInfo :: ShortId DM.Merchant -> Id Domain.Message -> Flow Common.MessageInfoResponse
messageInfo merchantShortId messageId = do
    _ <- findMerchantByShortId merchantShortId
    message <- MQuery.findById messageId >>= fromMaybeM (InvalidRequest "Message Not Found")
    mediaFiles <- mapMaybeM MFQuery.findById message.mediaFiles
    return $ buildMessageInfoResponse mediaFiles message 
    where
        buildMessageInfoResponse mf Domain.RawMessage {..} = 
            Common.MessageInfoResponse
                { messageId = cast id,
                  _type = toCommonType _type,
                  description,
                  title,
                  mediaFiles = (\mediaFile -> Common.MediaFile (toCommonMediaFileType mediaFile._type) (showBaseUrl mediaFile.url)) <$> mf
                }


messageDeliveryInfo :: ShortId DM.Merchant -> Id Domain.Message -> Flow Common.MessageDeliveryInfoResponse
messageDeliveryInfo merchantShortId messageId = do
    _ <- findMerchantByShortId merchantShortId
    success <- Esq.runInReplica $ MRQuery.getMessageCountByStatus messageId Domain.Success
    failed <- Esq.runInReplica $ MRQuery.getMessageCountByStatus messageId Domain.Failed
    pending <- Esq.runInReplica $ MRQuery.getMessageCountByStatus messageId Domain.Pending

    return $ Common.MessageDeliveryInfoResponse { messageId = cast messageId, success, failed, pending }

messageReceiverList :: ShortId DM.Merchant -> Id Domain.Message -> Maybe Text -> Maybe Common.MessageDeliveryStatus -> Maybe Int -> Maybe Int -> Flow Common.MessageReceiverListResponse
messageReceiverList merchantShortId msgId _ mbStatus mbLimit mbOffset = do 
    _ <- findMerchantByShortId merchantShortId
    messageReports <- Esq.runInReplica $ MRQuery.findByMessageIdAndStatusWithLimitAndOffset mbLimit mbOffset msgId $ toDomainDeliveryStatusType <$> mbStatus
    let count = length messageReports
    let summary = Common.Summary {totalCount = count, count}

    return $ Common.MessageReceiverListResponse { receivers = buildReceiverListItem <$> messageReports, summary }
    where 
        buildReceiverListItem Domain.MessageReport {..} = do 
            Common.MessageReceiverListItem
                { receiverId = cast driverId 
                , receiverName = "" 
                , receiverNumber = "" 
                , reply
                , seen = Just readStatus 
                , status = toCommonDeliveryStatusType deliveryStatus
                }
