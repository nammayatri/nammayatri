{-# LANGUAGE DerivingStrategies #-}

module IssueManagement.Domain.Action.UI.Issue where

import qualified AWS.S3 as S3
import AWS.S3.Types (S3Env)
import qualified Data.ByteString as BS
import Data.Text as T hiding (last, map, null)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified EulerHS.Language as L
import EulerHS.Prelude (withFile)
import EulerHS.Types (base64Encode)
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (..))
import IssueManagement.Common as Reexport
import qualified IssueManagement.Common.UI.Issue as Common
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as D
import qualified IssueManagement.Domain.Types.Issue.IssueConfig as D
import qualified IssueManagement.Domain.Types.Issue.IssueMessage as D
import qualified IssueManagement.Domain.Types.Issue.IssueOption as D
import qualified IssueManagement.Domain.Types.Issue.IssueReport as D
import qualified IssueManagement.Domain.Types.Issue.IssueTranslation as D
import qualified IssueManagement.Domain.Types.MediaFile as D
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueCategory as CQIC
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueConfig as CQI
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueMessage as CQIM
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueOption as CQIO
import qualified IssueManagement.Storage.CachedQueries.MediaFile as CQMF
import qualified IssueManagement.Storage.Queries.Issue.IssueReport as QIR
import qualified IssueManagement.Storage.Queries.MediaFile as QMF
import IssueManagement.Tools.Error
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Ticket.Interface.Types as TIT
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common

data ServiceHandle m = ServiceHandle
  { findRideById :: Id Ride -> m (Maybe Ride),
    findPersonById :: Id Person -> m (Maybe Person),
    findMerchant :: Id Merchant -> m (Maybe Merchant),
    getRideInfo :: ShortId Merchant -> Id Ride -> m RideInfoRes,
    createTicket :: Id Merchant -> TIT.CreateTicketReq -> m TIT.CreateTicketResp
  }

getLanguage :: EsqDBReplicaFlow m r => Id Person -> Maybe Language -> ServiceHandle m -> m Language
getLanguage personId mbLanguage issueHandle = do
  extractLanguage <-
    if isJust mbLanguage
      then return mbLanguage
      else runMaybeT $ do
        personDetail <- MaybeT $ issueHandle.findPersonById personId
        MaybeT $ pure personDetail.language
  return $ fromMaybe ENGLISH extractLanguage

getIssueCategory ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    CacheFlow m r
  ) =>
  (Id Person, Id Merchant) ->
  Maybe Language ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueCategoryListRes
getIssueCategory (personId, _) mbLanguage issueHandle identifier = do
  language <- getLanguage personId mbLanguage issueHandle
  issueCategoryTranslationList <- CQIC.findAllByLanguage language identifier
  pure $ Common.IssueCategoryListRes {categories = mkIssueCategory <$> issueCategoryTranslationList}
  where
    mkIssueCategory :: (D.IssueCategory, Maybe D.IssueTranslation) -> Common.IssueCategoryRes
    mkIssueCategory (issueCategory, issueTranslation) =
      Common.IssueCategoryRes
        { issueCategoryId = cast issueCategory.id,
          label = issueCategory.category & T.toUpper & T.replace " " "_",
          category = fromMaybe issueCategory.category $ issueTranslation <&> (.translation),
          logoUrl = issueCategory.logoUrl
        }

getIssueOption ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    CacheFlow m r
  ) =>
  (Id Person, Id Merchant) ->
  Id D.IssueCategory ->
  Maybe (Id D.IssueOption) ->
  Maybe (Id D.IssueReport) ->
  Maybe Language ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueOptionListRes
getIssueOption (personId, _) issueCategoryId issueOptionId issueReportId mbLanguage issueHandle identifier = do
  language <- getLanguage personId mbLanguage issueHandle
  issueMessageTranslationList <- case issueOptionId of
    Nothing -> CQIM.findAllByCategoryIdAndLanguage issueCategoryId language identifier
    Just optionId -> CQIM.findAllByOptionIdAndLanguage optionId language identifier
  let issueMessages = mkIssueMessageList $ Just issueMessageTranslationList
  issueOptionTranslationList <- do
    case (issueOptionId, issueReportId) of
      (Just optionId, Just iReportId) -> do
        issueReport <- QIR.findById iReportId >>= fromMaybeM (IssueReportDoNotExist iReportId.getId)
        now <- getCurrentTime
        let updatedChats =
              issueReport.chats
                ++ ( Chat
                       { chatType = IssueOption,
                         chatId = optionId.getId,
                         timestamp = now
                       } :
                     map
                       ( \message ->
                           Chat
                             { chatType = IssueMessage,
                               chatId = message.id.getId,
                               timestamp = now
                             }
                       )
                       issueMessages
                   )
        QIR.updateChats iReportId updatedChats
      _ -> return ()
    if null issueMessages
      then pure []
      else CQIO.findAllByMessageAndLanguage ((.id) $ last issueMessages) language identifier
  pure $
    Common.IssueOptionListRes
      { options = mkIssueOptionList <$> issueOptionTranslationList,
        messages = issueMessages
      }

issueReportList ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  (Id Person, Id Merchant) ->
  Maybe Language ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueReportListRes
issueReportList (personId, _) mbLanguage issueHandle identifier = do
  language <- getLanguage personId mbLanguage issueHandle
  issueReports <- QIR.findAllByPerson personId
  issueConfig <- CQI.findIssueConfig identifier >>= fromMaybeM (InternalError "IssueConfigNotFound")
  now <- getCurrentTime
  issues <- mapM (processIssueReport issueConfig now identifier language) issueReports
  return $ Common.IssueReportListRes {issues}
  where
    processIssueReport ::
      ( CacheFlow m r,
        BeamFlow m r
      ) =>
      D.IssueConfig ->
      UTCTime ->
      Identifier ->
      Language ->
      D.IssueReport ->
      m Common.IssueReportListItem
    processIssueReport iConfig currTime identifier_ language iReport = do
      let timeDiff = realToFrac (currTime `diffUTCTime` iReport.updatedAt) / 3600
      if iReport.status == RESOLVED && timeDiff > iConfig.autoMarkIssueClosedDuration
        then do
          QIR.updateStatusAssignee iReport.id (Just CLOSED) iReport.assignee
          mbIssueMessages <- mapM (`CQIM.findById` identifier_) iConfig.onAutoMarkIssueClsMsgs
          let issueMessages = mapMaybe ((.id) <$>) mbIssueMessages
          let updatedChats =
                iReport.chats
                  ++ map
                    ( \messageId ->
                        Chat
                          { chatType = IssueMessage,
                            chatId = messageId.getId,
                            timestamp = currTime
                          }
                    )
                    issueMessages
          QIR.updateChats iReport.id updatedChats
          mkIssueReport iReport (Just CLOSED) language
        else mkIssueReport iReport Nothing language

    mkIssueReport ::
      ( CacheFlow m r,
        BeamFlow m r
      ) =>
      D.IssueReport ->
      Maybe IssueStatus ->
      Language ->
      m Common.IssueReportListItem
    mkIssueReport issueReport issueStatus language = do
      (issueCategory, issueCategoryTranslation) <- CQIC.findByIdAndLanguage issueReport.categoryId language identifier >>= fromMaybeM (IssueCategoryNotFound issueReport.categoryId.getId)
      return $
        Common.IssueReportListItem
          { issueReportId = cast issueReport.id,
            category = fromMaybe issueCategory.category $ issueCategoryTranslation <&> (.translation),
            status = fromMaybe issueReport.status issueStatus,
            createdAt = issueReport.createdAt
          }

createFilePath ::
  ( BeamFlow m r,
    MonadTime m,
    MonadReader r m,
    HasField "s3Env" r (S3Env m)
  ) =>
  Text ->
  Common.FileType ->
  Text ->
  m Text
createFilePath personId fileType validatedFileExtention = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return
    ( pathPrefix <> "issue-media/" <> "driver-" <> personId <> "/"
        <> show fileType
        <> "/"
        <> fileName
        <> validatedFileExtention
    )

createMediaEntry :: BeamFlow m r => Text -> Common.FileType -> m Common.IssueMediaUploadRes
createMediaEntry url fileType = do
  fileEntity <- mkFile url
  _ <- QMF.create fileEntity
  return $ Common.IssueMediaUploadRes {fileId = cast $ fileEntity.id}
  where
    mapToMediaFileType = \case
      Common.Audio -> D.Audio
      Common.Image -> D.Image
    mkFile fileUrl = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        D.MediaFile
          { id,
            _type = mapToMediaFileType fileType,
            url = fileUrl,
            createdAt = now
          }

issueMediaUpload ::
  ( BeamFlow m r,
    MonadTime m,
    MonadReader r m,
    HasField "s3Env" r (S3Env m)
  ) =>
  (Id Person, Id Merchant) ->
  Common.IssueMediaUploadReq ->
  m Common.IssueMediaUploadConfig ->
  m Common.IssueMediaUploadRes
issueMediaUpload (personId, _) Common.IssueMediaUploadReq {..} issueMediaUploadConfig = do
  contentType <- validateContentType
  config <- issueMediaUploadConfig
  fileSize <- L.runIO $ withFile file ReadMode hFileSize
  when (fileSize > fromIntegral config.mediaFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile file
  filePath <- createFilePath personId.getId fileType contentType
  let fileUrl =
        config.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "issue"
          & T.replace "<FILE_PATH>" filePath
  _ <- fork "S3 Put Issue Media File" $ S3.put (T.unpack filePath) mediaFile
  createMediaEntry fileUrl fileType
  where
    validateContentType = do
      case fileType of
        Common.Audio | reqContentType == "audio/wave" -> pure "wav"
        Common.Audio | reqContentType == "audio/mpeg" -> pure "mp3"
        Common.Audio | reqContentType == "audio/mp4" -> pure "mp4"
        Common.Image | reqContentType == "image/png" -> pure "png"
        Common.Image | reqContentType == "image/jpeg" -> pure "jpg"
        _ -> throwError $ FileFormatNotSupported reqContentType

fetchMedia :: (HasField "s3Env" r (S3Env m), MonadReader r m, BeamFlow m r) => (Id Person, Id Merchant) -> Text -> m Text
fetchMedia _personId filePath =
  S3.get $ T.unpack filePath

createIssueReport ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    BeamFlow m r
  ) =>
  (Id Person, Id Merchant) ->
  Maybe Language ->
  Common.IssueReportReq ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueReportRes
createIssueReport (personId, merchantId) mbLanguage Common.IssueReportReq {..} issueHandle identifier = do
  category <- CQIC.findById (cast categoryId) identifier >>= fromMaybeM (IssueCategoryDoNotExist categoryId.getId)
  mbOption <- forM optionId \justOptionId -> do
    CQIO.findByIdAndCategoryId (cast justOptionId) (cast categoryId) identifier >>= fromMaybeM (IssueOptionInvalid justOptionId.getId categoryId.getId)
  mbRide <- forM rideId \justRideId -> do
    B.runInReplica (issueHandle.findRideById $ cast justRideId) >>= fromMaybeM (RideNotFound justRideId.getId)
  uploadedMediaFiles <- forM mediaFiles $ \mediaFile ->
    CQMF.findById (cast mediaFile) identifier >>= fromMaybeM (FileDoNotExist mediaFile.getId)
  let mediaFileUrls = map (.url) uploadedMediaFiles
  language <- getLanguage personId mbLanguage issueHandle
  issueConfig <- CQI.findIssueConfig identifier >>= fromMaybeM (InternalError "IssueConfigNotFound")
  issueMessageTranslationList <- mapM (\messageId -> CQIM.findByIdAndLanguage messageId language identifier) issueConfig.onCreateIssueMsgs
  let messages = mkIssueMessageList $ sequence issueMessageTranslationList
  now <- getCurrentTime
  let chats_ = fromMaybe [] chats
  let updatedChats =
        chats_
          ++ [ Chat
                 { chatId = "",
                   timestamp = now,
                   chatType = IssueDescription
                 }
             ]
          ++ map
            ( \mediaFile ->
                Chat
                  { chatId = mediaFile.id.getId,
                    timestamp = now,
                    chatType = MediaFile
                  }
            )
            uploadedMediaFiles
          ++ map
            ( \message ->
                Chat
                  { chatId = message.id.getId,
                    timestamp = now,
                    chatType = IssueMessage
                  }
            )
            messages
  issueReport <- mkIssueReport updatedChats
  _ <- QIR.create issueReport
  merchant <- issueHandle.findMerchant merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  ticket <- buildTicket issueReport category mbOption mbRide merchant.shortId mediaFileUrls issueHandle identifier
  ticketResponse <- try @_ @SomeException (issueHandle.createTicket merchantId ticket)
  case ticketResponse of
    Right ticketResponse' -> do
      QIR.updateTicketId issueReport.id ticketResponse'.ticketId
    Left err -> do
      logTagInfo "Create Ticket API failed - " $ show err
  pure $ Common.IssueReportRes {issueReportId = cast issueReport.id, messages}
  where
    mkIssueReport updatedChats = do
      id <- generateGUID
      now <- getCurrentTime
      pure $
        D.IssueReport
          { id,
            personId,
            driverId = if identifier == CUSTOMER then Nothing else Just personId,
            rideId = cast <$> rideId,
            optionId = cast <$> optionId,
            categoryId = cast categoryId,
            mediaFiles = cast <$> mediaFiles,
            assignee = Nothing,
            status = OPEN,
            deleted = False,
            ticketId = Nothing,
            createdAt = now,
            updatedAt = now,
            description,
            chats = updatedChats
          }

    buildTicket :: (CacheFlow m r, EsqDBReplicaFlow m r, EncFlow m r, BeamFlow m r) => D.IssueReport -> D.IssueCategory -> Maybe D.IssueOption -> Maybe Ride -> ShortId Merchant -> [Text] -> ServiceHandle m -> Identifier -> m TIT.CreateTicketReq
    buildTicket issue category mbOption mbRide merchantShortId mediaFileUrls issueServiceHandle identifier_ = do
      info <- forM mbRide (buildRideInfo merchantShortId issueServiceHandle)
      person <- issueServiceHandle.findPersonById personId >>= fromMaybeM (PersonNotFound personId.getId)
      phoneNumber <- mapM decrypt person.mobileNumber
      return $
        TIT.CreateTicketReq
          { category = category.category,
            subCategory = (.option) <$> mbOption,
            issueId = Just issue.id.getId,
            issueDescription = description,
            mediaFiles = Just mediaFileUrls,
            name = Just $ fromMaybe "" person.firstName <> " " <> fromMaybe "" person.lastName,
            phoneNo = phoneNumber,
            personId = person.id.getId,
            classification = castIdentifierToClassification identifier_,
            rideDescription = info
          }

    buildRideInfo :: (CacheFlow m r, EsqDBReplicaFlow m r, BeamFlow m r) => ShortId Merchant -> ServiceHandle m -> Ride -> m TIT.RideInfo
    buildRideInfo merchantShortId issueServiceHandle ride = do
      res <- issueServiceHandle.getRideInfo merchantShortId (cast ride.id)
      return
        TIT.RideInfo
          { rideShortId = ride.shortId.getShortId,
            customerName = res.customerName,
            customerPhoneNo = Just res.customerPhoneNo,
            driverName = Just res.driverName,
            driverPhoneNo = res.driverPhoneNo,
            vehicleNo = res.vehicleNo,
            status = show res.bookingStatus,
            rideCreatedAt = ride.createdAt,
            pickupLocation = mkLocation res.customerPickupLocation,
            dropLocation = mkLocation <$> res.customerDropLocation,
            fare = res.actualFare
          }

    mkLocation Common.LocationAPIEntity {..} = TIT.Location {..}

    castIdentifierToClassification :: Identifier -> TIT.Classification
    castIdentifierToClassification = \case
      DRIVER -> TIT.DRIVER
      CUSTOMER -> TIT.CUSTOMER

issueInfo ::
  ( EsqDBReplicaFlow m r,
    CacheFlow m r,
    BeamFlow m r
  ) =>
  Id D.IssueReport ->
  (Id Person, Id Merchant) ->
  Maybe Language ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueInfoRes
issueInfo issueReportId (personId, _) mbLanguage issueHandle identifier = do
  language <- getLanguage personId mbLanguage issueHandle
  issueReport <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  mediaFiles <- CQMF.findAllInForIssueReportId issueReport.mediaFiles issueReportId identifier
  (issueCategory, _) <- CQIC.findByIdAndLanguage issueReport.categoryId language identifier >>= fromMaybeM (IssueCategoryNotFound issueReport.categoryId.getId)
  mbIssueOption <- (join <$>) $
    forM issueReport.optionId $ \justIssueOption -> do
      CQIO.findByIdAndLanguage justIssueOption language identifier
  issueChats <-
    mapM
      ( \item -> case item.chatType of
          IssueMessage -> do
            mbIssueMessageTranslation <- CQIM.findByIdAndLanguage (Id item.chatId) language identifier
            pure $
              Common.ChatDetail
                { id = item.chatId,
                  content = mkIssueMessage <$> mbIssueMessageTranslation,
                  timestamp = item.timestamp,
                  sender = BOT,
                  chatType = Text,
                  label = (\(issueMsg, _) -> issueMsg.label) =<< mbIssueMessageTranslation
                }
          IssueOption -> do
            mbIssueOptionTranslation <- CQIO.findByIdAndLanguage (Id item.chatId) language identifier
            pure $
              Common.ChatDetail
                { id = item.chatId,
                  content = mkIssueOption <$> mbIssueOptionTranslation,
                  timestamp = item.timestamp,
                  chatType = Text,
                  sender = USER,
                  label = (\(issueOpt, _) -> issueOpt.label) =<< mbIssueOptionTranslation
                }
          IssueDescription -> do
            pure $
              Common.ChatDetail
                { id = item.chatId,
                  content = Just issueReport.description,
                  timestamp = item.timestamp,
                  chatType = Text,
                  sender = USER,
                  label = Nothing
                }
          MediaFile -> do
            mediaFile <- CQMF.findById (Id item.chatId) identifier >>= fromMaybeM (FileDoNotExist item.chatId)
            pure $
              Common.ChatDetail
                { id = item.chatId,
                  content = Just mediaFile.url,
                  timestamp = item.timestamp,
                  chatType = mediaTypeToMessageType mediaFile._type,
                  sender = USER,
                  label = Nothing
                }
      )
      issueReport.chats
  issueOptions <-
    if null issueReport.chats
      then pure []
      else CQIO.findAllByMessageAndLanguage (Id (last issueReport.chats).chatId) language identifier
  pure $
    Common.IssueInfoRes
      { issueReportId = cast issueReport.id,
        categoryLabel = issueCategory.category & T.toUpper & T.replace " " "_",
        option = mkIssueOption <$> mbIssueOption,
        assignee = issueReport.assignee,
        description = issueReport.description,
        status = issueReport.status,
        mediaFiles = mkMediaFiles mediaFiles,
        createdAt = issueReport.createdAt,
        chats = issueChats,
        options = map mkIssueOptionList issueOptions,
        categoryId = issueReport.categoryId
      }
  where
    mkIssueOption :: (D.IssueOption, Maybe D.IssueTranslation) -> Text
    mkIssueOption (issueOption, issueOptionTranslation) =
      fromMaybe issueOption.option $ (.translation) <$> issueOptionTranslation

    mkMediaFiles :: [D.MediaFile] -> [Common.MediaFile_]
    mkMediaFiles =
      foldr'
        ( \mediaFile mediaFileList -> do
            case mediaFile._type of
              D.Audio -> Common.MediaFile_ Common.Audio mediaFile.url : mediaFileList
              D.Image -> Common.MediaFile_ Common.Image mediaFile.url : mediaFileList
              _ -> mediaFileList
        )
        []

    mediaTypeToMessageType :: D.MediaType -> MessageType
    mediaTypeToMessageType = \case
      D.Audio -> Audio
      D.Image -> Image
      _ -> Text

    mkIssueMessage :: (D.IssueMessage, Maybe D.IssueTranslation) -> Text
    mkIssueMessage (issueMessage, issueOptionTranslation) =
      fromMaybe issueMessage.message $ (.translation) <$> issueOptionTranslation

updateIssueOption ::
  ( CacheFlow m r,
    BeamFlow m r
  ) =>
  Id D.IssueReport ->
  (Id Person, Id Merchant) ->
  Common.IssueUpdateReq ->
  Identifier ->
  m APISuccess
updateIssueOption issueReportId (_, _) Common.IssueUpdateReq {..} identifier = do
  void $ QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  void $ CQIO.findByIdAndCategoryId (cast optionId) (cast categoryId) identifier >>= fromMaybeM (IssueOptionInvalid optionId.getId categoryId.getId)
  _ <- QIR.updateOption issueReportId (cast optionId)
  pure Success

deleteIssue ::
  ( EsqDBReplicaFlow m r,
    CacheFlow m r,
    BeamFlow m r
  ) =>
  Id D.IssueReport ->
  (Id Person, Id Merchant) ->
  Identifier ->
  m APISuccess
deleteIssue issueReportId (personId, _) identifier = do
  unlessM (B.runInReplica (QIR.isSafeToDelete issueReportId personId)) $
    throwError (InvalidRequest "This issue is either already deleted, or is not associated to this person.")
  issueReport <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  _ <- QIR.updateAsDeleted issueReportId
  CQMF.invalidateMediaFileCache issueReport.mediaFiles (Just issueReportId) identifier
  pure Success

updateIssueStatus ::
  ( EsqDBReplicaFlow m r,
    CacheFlow m r,
    BeamFlow m r
  ) =>
  (Id Person, Id Merchant) ->
  Id D.IssueReport ->
  Maybe Language ->
  Common.IssueStatusUpdateReq ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueStatusUpdateRes
updateIssueStatus (personId, _) issueReportId mbLanguage Common.IssueStatusUpdateReq {..} issueHandle identifier = do
  language <- getLanguage personId mbLanguage issueHandle
  issueReport <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  case status of
    CLOSED -> do
      QIR.updateStatusAssignee issueReport.id (Just status) issueReport.assignee
      pure $
        Common.IssueStatusUpdateRes
          { messages = []
          }
    REOPENED -> do
      QIR.updateStatusAssignee issueReport.id (Just status) issueReport.assignee
      issueConfig <- CQI.findIssueConfig identifier >>= fromMaybeM (InternalError "IssueConfigNotFound")
      issueMessageTranslation <- mapM (\messageId -> CQIM.findByIdAndLanguage messageId language identifier) issueConfig.onIssueReopenMsgs
      let issueMessages = mkIssueMessageList $ sequence issueMessageTranslation
      now <- getCurrentTime
      let updatedChats =
            issueReport.chats
              ++ map
                ( \message ->
                    Chat
                      { chatType = IssueMessage,
                        chatId = message.id.getId,
                        timestamp = now
                      }
                )
                issueMessages
      QIR.updateChats issueReportId updatedChats
      pure $
        Common.IssueStatusUpdateRes
          { messages = issueMessages
          }
    _ -> throwError $ InternalError "Cannot Update Issue : Incorrect Status Provided"

mkIssueMessageList ::
  Maybe [(D.IssueMessage, Maybe D.IssueTranslation)] -> [Common.Message]
mkIssueMessageList mbList = case mbList of
  Nothing -> []
  Just list ->
    map
      ( \(issueMessage, issueTranslation) ->
          Common.Message
            { id = issueMessage.id,
              message = fromMaybe issueMessage.message $ issueTranslation <&> (.translation)
            }
      )
      list

mkIssueOptionList ::
  (D.IssueOption, Maybe D.IssueTranslation) -> Common.IssueOptionRes
mkIssueOptionList (issueOption, issueTranslation) =
  Common.IssueOptionRes
    { issueOptionId = cast (issueOption.id),
      label = fromMaybe (issueOption.option & T.toUpper & T.replace " " "_") issueOption.label,
      option = fromMaybe issueOption.option $ issueTranslation <&> (.translation)
    }
