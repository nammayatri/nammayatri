{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.Issue where

import qualified AWS.S3 as S3
import AWS.S3.Types (S3Env)
import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as DCommon
import qualified Data.ByteString as BS
import Data.Text as T hiding (last, map, null)
import Data.Time.Format.ISO8601 (iso8601Show)
import Domain.Action.Dashboard.Ride as DRide
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import qualified EulerHS.Language as L
import EulerHS.Prelude (withFile)
import EulerHS.Types (base64Encode)
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (..))
import qualified IssueManagement.Common as Domain
import qualified IssueManagement.Common.UI.Issue as Common
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as D
import qualified IssueManagement.Domain.Types.Issue.IssueConfig as D
import qualified IssueManagement.Domain.Types.Issue.IssueMessage as D
import qualified IssueManagement.Domain.Types.Issue.IssueOption as D
import qualified IssueManagement.Domain.Types.Issue.IssueReport as D
import qualified IssueManagement.Domain.Types.Issue.IssueTranslation as D
import qualified IssueManagement.Domain.Types.MediaFile as D
import IssueManagement.Storage.CachedQueries.CacheConfig
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
import Storage.Beam.IssueManagement ()
import Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QR
import qualified Tools.Ticket as TT

getLanguage :: EsqDBReplicaFlow m r => Id DP.Person -> Maybe Language -> m Language
getLanguage personId mbLanguage = do
  extractLanguage <-
    if isJust mbLanguage
      then return mbLanguage
      else runMaybeT $ do
        personDetail <- MaybeT $ B.runInReplica $ QP.findById (cast personId)
        MaybeT $ pure personDetail.language
  return $ fromMaybe ENGLISH extractLanguage

getIssueCategory ::
  ( MonadFlow m,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    CacheFlow m r,
    Esq.EsqDBFlow m r
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  Maybe Language ->
  Common.Identifier ->
  m Common.IssueCategoryListRes
getIssueCategory (personId, _) mbLanguage identifier = do
  language <- getLanguage personId mbLanguage
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
  ( EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    CacheFlow m r,
    Esq.EsqDBFlow m r
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  Id D.IssueCategory ->
  Maybe (Id D.IssueOption) ->
  Maybe (Id D.IssueReport) ->
  Maybe Language ->
  Common.Identifier ->
  m Common.IssueOptionListRes
getIssueOption (personId, _) issueCategoryId issueOptionId issueReportId mbLanguage identifier = do
  language <- getLanguage personId mbLanguage
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
                ++ ( Common.Chat
                       { chatType = Domain.IssueOption,
                         chatId = optionId.getId,
                         timestamp = now
                       } :
                     map
                       ( \message ->
                           Common.Chat
                             { chatType = Domain.IssueMessage,
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
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  Maybe Language ->
  Common.Identifier ->
  m Common.IssueReportListRes
issueReportList (personId, _) mbLanguage identifier = do
  language <- getLanguage personId mbLanguage
  issueReports <- QIR.findAllByPerson (cast personId)
  issueConfig <- CQI.findIssueConfig identifier >>= fromMaybeM (InternalError "IssueConfigNotFound")
  now <- getCurrentTime
  issues <- mapM (processIssueReport issueConfig now identifier language) issueReports
  return $ Common.IssueReportListRes {issues}
  where
    processIssueReport ::
      ( CacheFlow m r,
        Esq.EsqDBFlow m r
      ) =>
      D.IssueConfig ->
      UTCTime ->
      Common.Identifier ->
      Language ->
      D.IssueReport ->
      m Common.IssueReportListItem
    processIssueReport iConfig currTime identifier_ language iReport = do
      let timeDiff = realToFrac (currTime `diffUTCTime` iReport.updatedAt) / 3600
      if iReport.status == Domain.AWAIT && timeDiff > iConfig.autoMarkIssueResolveDuration
        then do
          QIR.updateStatusAssignee iReport.id (Just Common.RESOLVED) iReport.assignee
          mbIssueMessages <- mapM (`CQIM.findById` identifier_) iConfig.onAutoMarkIssueResMsgs
          let issueMessages = mapMaybe ((.id) <$>) mbIssueMessages
          let updatedChats =
                iReport.chats
                  ++ map
                    ( \messageId ->
                        Common.Chat
                          { chatType = Domain.IssueMessage,
                            chatId = messageId.getId,
                            timestamp = currTime
                          }
                    )
                    issueMessages
          QIR.updateChats iReport.id updatedChats
          mkIssueReport iReport (Just Common.RESOLVED) language
        else mkIssueReport iReport Nothing language

    mkIssueReport ::
      ( CacheFlow m r,
        Esq.EsqDBFlow m r
      ) =>
      D.IssueReport ->
      Maybe Common.IssueStatus ->
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
  ( MonadTime m,
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

createMediaEntry :: Esq.EsqDBFlow m r => Text -> Common.FileType -> m Common.IssueMediaUploadRes
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
  ( MonadTime m,
    MonadReader r m,
    HasField "s3Env" r (S3Env m),
    L.MonadFlow m,
    Esq.EsqDBFlow m r,
    CacheFlow m r
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  Common.IssueMediaUploadReq ->
  m Common.IssueMediaUploadRes
issueMediaUpload (personId, merchantId) Common.IssueMediaUploadReq {..} = do
  contentType <- validateContentType
  merchant <- CQM.findById (cast merchantId) >>= fromMaybeM (MerchantNotFound merchantId.getId)
  fileSize <- L.runIO $ withFile file ReadMode hFileSize
  when (fileSize > fromIntegral merchant.mediaFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile file
  filePath <- createFilePath personId.getId fileType contentType
  let fileUrl =
        merchant.mediaFileUrlPattern
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

fetchMedia :: (HasField "s3Env" r (S3Env m), MonadReader r m) => (Id DP.Person, Id DM.Merchant) -> Text -> m Text
fetchMedia _personId filePath =
  S3.get $ T.unpack filePath

createIssueReport ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  Maybe Language ->
  Common.IssueReportReq ->
  Common.Identifier ->
  m Common.IssueReportRes
createIssueReport (personId, merchantId) mbLanguage Common.IssueReportReq {..} identifier = do
  category <- CQIC.findById (cast categoryId) identifier >>= fromMaybeM (IssueCategoryDoNotExist categoryId.getId)
  mbOption <- forM optionId \justOptionId -> do
    CQIO.findByIdAndCategoryId (cast justOptionId) (cast categoryId) identifier >>= fromMaybeM (IssueOptionInvalid justOptionId.getId categoryId.getId)
  mbRide <- forM rideId \justRideId -> do
    B.runInReplica (QR.findById $ cast justRideId) >>= fromMaybeM (RideNotFound justRideId.getId)
  uploadedMediaFiles <- forM mediaFiles $ \mediaFile ->
    CQMF.findById (cast mediaFile) identifier >>= fromMaybeM (FileDoNotExist mediaFile.getId)
  let mediaFileUrls = map (.url) uploadedMediaFiles
  language <- getLanguage personId mbLanguage
  issueConfig <- CQI.findIssueConfig identifier >>= fromMaybeM (InternalError "IssueConfigNotFound")
  issueMessageTranslationList <- mapM (\messageId -> CQIM.findByIdAndLanguage messageId language identifier) issueConfig.onCreateIssueMsgs
  let messages = mkIssueMessageList $ sequence issueMessageTranslationList
  now <- getCurrentTime
  let updatedChats =
        chats
          ++ [ Common.Chat
                 { chatId = "",
                   timestamp = now,
                   chatType = Domain.IssueDescription
                 }
             ]
          ++ map
            ( \mediaFile ->
                Common.Chat
                  { chatId = mediaFile.id.getId,
                    timestamp = now,
                    chatType = Domain.MediaFile
                  }
            )
            uploadedMediaFiles
          ++ map
            ( \message ->
                Common.Chat
                  { chatId = message.id.getId,
                    timestamp = now,
                    chatType = Domain.IssueMessage
                  }
            )
            messages
  issueReport <- mkIssueReport updatedChats
  _ <- QIR.create issueReport
  merchant <- CQM.findById (cast merchantId) >>= fromMaybeM (MerchantNotFound merchantId.getId)
  ticket <- buildTicket issueReport category mbOption mbRide merchant.shortId mediaFileUrls
  ticketResponse <- TT.createTicket (cast merchantId) ticket
  QIR.updateTicketId issueReport.id ticketResponse.ticketId
  pure $ Common.IssueReportRes {issueReportId = cast issueReport.id, messages}
  where
    mkIssueReport updatedChats = do
      id <- generateGUID
      now <- getCurrentTime
      pure $
        D.IssueReport
          { id,
            personId = cast personId,
            rideId = cast <$> rideId,
            optionId = cast <$> optionId,
            categoryId = cast categoryId,
            mediaFiles = cast <$> mediaFiles,
            assignee = Nothing,
            status = Domain.OPEN,
            deleted = False,
            ticketId = Nothing,
            createdAt = now,
            updatedAt = now,
            description,
            chats = updatedChats
          }

    buildTicket :: (CacheFlow m r, Esq.EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => D.IssueReport -> D.IssueCategory -> Maybe D.IssueOption -> Maybe DR.Ride -> ShortId DM.Merchant -> [Text] -> m TIT.CreateTicketReq
    buildTicket issue category mbOption mbRide merchantShortId mediaFileUrls = do
      info <- forM mbRide (buildRideInfo merchantShortId)
      person <- B.runInReplica $ QP.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
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
            classification = TIT.CUSTOMER,
            rideDescription = info
          }

    buildRideInfo ::
      ( CacheFlow m r,
        Esq.EsqDBFlow m r,
        EsqDBReplicaFlow m r,
        EncFlow m r
      ) =>
      ShortId DM.Merchant ->
      DR.Ride ->
      m TIT.RideInfo
    buildRideInfo (ShortId merchantShortId) ride = do
      res <- DRide.rideInfo (ShortId merchantShortId) (cast ride.id)
      return
        TIT.RideInfo
          { rideShortId = ride.shortId.getShortId,
            customerName = res.customerName,
            customerPhoneNo = res.customerPhoneNo,
            driverName = Just res.driverName,
            driverPhoneNo = res.driverPhoneNo,
            vehicleNo = res.vehicleNo,
            status = show res.rideStatus,
            rideCreatedAt = ride.createdAt,
            pickupLocation = mkLocation res.customerPickupLocation,
            dropLocation = mkLocation <$> res.customerDropLocation,
            fare = res.actualFare
          }

    mkLocation DCommon.Location {..} =
      TIT.Location
        { street = address.street,
          area = address.state,
          city = address.city,
          state = address.state,
          country = address.country,
          building = address.building,
          areaCode = address.areaCode,
          ..
        }

issueInfo ::
  ( EsqDBReplicaFlow m r,
    CacheFlow m r,
    Esq.EsqDBFlow m r
  ) =>
  Id D.IssueReport ->
  (Id DP.Person, Id DM.Merchant) ->
  Maybe Language ->
  Common.Identifier ->
  m Common.IssueInfoRes
issueInfo issueReportId (personId, _) mbLanguage identifier = do
  language <- getLanguage personId mbLanguage
  issueReport <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  mediaFiles <- CQMF.findAllInForIssueReportId issueReport.mediaFiles issueReportId identifier
  (issueCategory, _) <- CQIC.findByIdAndLanguage issueReport.categoryId language identifier >>= fromMaybeM (IssueCategoryNotFound issueReport.categoryId.getId)
  mbIssueOption <- (join <$>) $
    forM issueReport.optionId $ \justIssueOption -> do
      CQIO.findByIdAndLanguage justIssueOption language identifier
  issueChats <-
    mapM
      ( \item -> case item.chatType of
          Domain.IssueMessage -> do
            mbIssueMessageTranslation <- CQIM.findByIdAndLanguage (Id item.chatId) language identifier
            pure $
              Common.ChatDetail
                { id = item.chatId,
                  content = mkIssueMessage <$> mbIssueMessageTranslation,
                  timestamp = item.timestamp,
                  sender = Domain.BOT,
                  chatType = Domain.Text,
                  label = (\(issueMsg, _) -> issueMsg.label) =<< mbIssueMessageTranslation
                }
          Domain.IssueOption -> do
            mbIssueOptionTranslation <- CQIO.findByIdAndLanguage (Id item.chatId) language identifier
            pure $
              Common.ChatDetail
                { id = item.chatId,
                  content = mkIssueOption <$> mbIssueOptionTranslation,
                  timestamp = item.timestamp,
                  chatType = Domain.Text,
                  sender = Domain.USER,
                  label = (\(issueOpt, _) -> issueOpt.label) =<< mbIssueOptionTranslation
                }
          Domain.IssueDescription -> do
            pure $
              Common.ChatDetail
                { id = item.chatId,
                  content = Just issueReport.description,
                  timestamp = item.timestamp,
                  chatType = Domain.Text,
                  sender = Domain.USER,
                  label = Nothing
                }
          Domain.MediaFile -> do
            mediaFile <- CQMF.findById (Id item.chatId) identifier >>= fromMaybeM (FileDoNotExist item.chatId)
            pure $
              Common.ChatDetail
                { id = item.chatId,
                  content = Just mediaFile.url,
                  timestamp = item.timestamp,
                  chatType = mediaTypeToMessageType mediaFile._type,
                  sender = Domain.USER,
                  label = Nothing
                }
      )
      issueReport.chats
  issueOptions <- CQIO.findAllByMessageAndLanguage (Id (last issueReport.chats).chatId) language identifier
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

    mediaTypeToMessageType :: D.MediaType -> Common.MessageType
    mediaTypeToMessageType = \case
      D.Audio -> Domain.Audio
      D.Image -> Domain.Image
      _ -> Domain.Text

    mkIssueMessage :: (D.IssueMessage, Maybe D.IssueTranslation) -> Text
    mkIssueMessage (issueMessage, issueOptionTranslation) =
      fromMaybe issueMessage.message $ (.translation) <$> issueOptionTranslation

updateIssueOption ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r
  ) =>
  Id D.IssueReport ->
  (Id DP.Person, Id DM.Merchant) ->
  Common.IssueUpdateReq ->
  Common.Identifier ->
  m APISuccess
updateIssueOption issueReportId (_, _) Common.IssueUpdateReq {..} identifier = do
  void $ QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  void $ CQIO.findByIdAndCategoryId (cast optionId) (cast categoryId) identifier >>= fromMaybeM (IssueOptionInvalid optionId.getId categoryId.getId)
  _ <- QIR.updateOption issueReportId (cast optionId)
  pure Success

deleteIssue ::
  ( EsqDBReplicaFlow m r,
    CacheFlow m r,
    Esq.EsqDBFlow m r
  ) =>
  Id D.IssueReport ->
  (Id DP.Person, Id DM.Merchant) ->
  Common.Identifier ->
  m APISuccess
deleteIssue issueReportId (personId, _) identifier = do
  unlessM (B.runInReplica (QIR.isSafeToDelete issueReportId (cast personId))) $
    throwError (InvalidRequest "This issue is either already deleted, or is not associated to this person.")
  issueReport <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  _ <- QIR.updateAsDeleted issueReportId
  CQMF.invalidateMediaFileCache issueReport.mediaFiles (Just issueReportId) identifier
  pure Success

updateIssueStatus ::
  ( EsqDBReplicaFlow m r,
    CacheFlow m r,
    Esq.EsqDBFlow m r
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  Id D.IssueReport ->
  Maybe Language ->
  Common.IssueStatusUpdateReq ->
  Common.Identifier ->
  m Common.IssueStatusUpdateRes
updateIssueStatus (personId, _) issueReportId mbLanguage Common.IssueStatusUpdateReq {..} identifier = do
  language <- getLanguage personId mbLanguage
  issueReport <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  case status of
    Common.RESOLVED -> do
      QIR.updateStatusAssignee issueReport.id (Just status) issueReport.assignee
      pure $
        Common.IssueStatusUpdateRes
          { messages = []
          }
    Common.REOPENED -> do
      QIR.updateStatusAssignee issueReport.id (Just status) issueReport.assignee
      issueConfig <- CQI.findIssueConfig identifier >>= fromMaybeM (InternalError "IssueConfigNotFound")
      issueMessageTranslation <- mapM (\messageId -> CQIM.findByIdAndLanguage messageId language identifier) issueConfig.onIssueReopenMsgs
      let issueMessages = mkIssueMessageList $ sequence issueMessageTranslation
      now <- getCurrentTime
      let updatedChats =
            issueReport.chats
              ++ map
                ( \message ->
                    Common.Chat
                      { chatType = Domain.IssueMessage,
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
