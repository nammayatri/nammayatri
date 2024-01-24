{-# LANGUAGE DerivingStrategies #-}

module IssueManagement.Domain.Action.UI.Issue where

import qualified AWS.S3 as S3
import qualified Data.ByteString as BS
import Data.Text as T hiding (last, map, null)
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
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common

data ServiceHandle m = ServiceHandle
  { findRideById :: Id Ride -> Id Merchant -> m (Maybe Ride),
    findPersonById :: Id Person -> m (Maybe Person),
    findMOCityById :: Id MerchantOperatingCity -> m (Maybe MerchantOperatingCity),
    getRideInfo :: Id Merchant -> Id MerchantOperatingCity -> Id Ride -> m RideInfoRes,
    createTicket :: Id Merchant -> Id MerchantOperatingCity -> TIT.CreateTicketReq -> m TIT.CreateTicketResp,
    updateTicket :: Id Merchant -> Id MerchantOperatingCity -> TIT.UpdateTicketReq -> m TIT.UpdateTicketResp,
    findMerchantConfig :: Id Merchant -> Id MerchantOperatingCity -> Id Person -> m MerchantConfig
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
    CoreMetrics m
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
        { issueCategoryId = issueCategory.id,
          label = issueCategory.category & T.toUpper & T.replace " " "_",
          category = fromMaybe issueCategory.category $ issueTranslation <&> (.translation),
          logoUrl = issueCategory.logoUrl
        }

getIssueOption ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
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
              issueReport.chats ++ (mkIssueChat IssueOption optionId.getId now) :
              map (\message -> mkIssueChat IssueMessage message.id.getId now) issueMessages
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
    EncFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  (Id Person, Id Merchant, Id MerchantOperatingCity) ->
  Maybe Language ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueReportListRes
issueReportList (personId, merchantId, merchantOpCityId) mbLanguage issueHandle identifier = do
  language <- getLanguage personId mbLanguage issueHandle
  issueReports <- QIR.findAllByPerson personId
  issueConfig <- CQI.findIssueConfig identifier >>= fromMaybeM (InternalError "IssueConfigNotFound")
  now <- getCurrentTime
  issues <- mapM (processIssueReport merchantId merchantOpCityId issueConfig now identifier language issueHandle) issueReports
  return $ Common.IssueReportListRes {issues}
  where
    processIssueReport ::
      ( BeamFlow m r,
        EncFlow m r
      ) =>
      Id Merchant ->
      Id MerchantOperatingCity ->
      D.IssueConfig ->
      UTCTime ->
      Identifier ->
      Language ->
      ServiceHandle m ->
      D.IssueReport ->
      m Common.IssueReportListItem
    processIssueReport mId moCityId iConfig currTime identifier_ language iHandle iReport = do
      let timeDiff = realToFrac (currTime `diffUTCTime` iReport.updatedAt) / 3600
      if iReport.status == RESOLVED && timeDiff > iConfig.autoMarkIssueClosedDuration
        then do
          QIR.updateStatusAssignee iReport.id (Just CLOSED) iReport.assignee
          updateTicketStatus iReport TIT.CL mId moCityId iHandle "Closed by system"
          mbIssueMessages <- mapM (`CQIM.findById` identifier_) iConfig.onAutoMarkIssueClsMsgs
          let issueMessages = mapMaybe ((.id) <$>) mbIssueMessages
          let updatedChats =
                iReport.chats ++ map (\messageId -> mkIssueChat IssueMessage messageId.getId currTime) issueMessages
          QIR.updateChats iReport.id updatedChats
          mkIssueReport iReport (Just CLOSED) language
        else mkIssueReport iReport Nothing language

    mkIssueReport ::
      BeamFlow m r =>
      D.IssueReport ->
      Maybe IssueStatus ->
      Language ->
      m Common.IssueReportListItem
    mkIssueReport issueReport issueStatus language = do
      (issueCategory, issueCategoryTranslation) <- CQIC.findByIdAndLanguage issueReport.categoryId language identifier >>= fromMaybeM (IssueCategoryNotFound issueReport.categoryId.getId)
      return $
        Common.IssueReportListItem
          { issueReportId = issueReport.id,
            issueReportShortId = issueReport.shortId,
            category = fromMaybe issueCategory.category $ issueCategoryTranslation <&> (.translation),
            status = fromMaybe issueReport.status issueStatus,
            createdAt = issueReport.createdAt
          }

createMediaEntry :: BeamFlow m r => Text -> S3.FileType -> m Common.IssueMediaUploadRes
createMediaEntry url fileType = do
  fileEntity <- mkFile url
  _ <- QMF.create fileEntity
  return $ Common.IssueMediaUploadRes {fileId = fileEntity.id}
  where
    mkFile fileUrl = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        D.MediaFile
          { id,
            _type = fileType,
            url = fileUrl,
            createdAt = now
          }

issueMediaUpload ::
  ( BeamFlow m r,
    MonadTime m,
    MonadReader r m,
    HasField "s3Env" r (S3.S3Env m),
    EsqDBReplicaFlow m r
  ) =>
  (Id Person, Id Merchant) ->
  ServiceHandle m ->
  Common.IssueMediaUploadReq ->
  m Common.IssueMediaUploadRes
issueMediaUpload (personId, merchantId) issueHandle Common.IssueMediaUploadReq {..} = do
  contentType <- validateContentType
  person <- issueHandle.findPersonById personId >>= fromMaybeM (PersonNotFound personId.getId)
  config <- issueHandle.findMerchantConfig merchantId person.merchantOperatingCityId personId
  fileSize <- L.runIO $ withFile file ReadMode hFileSize
  when (fileSize > fromIntegral config.mediaFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile file
  filePath <- S3.createFilePath "issue-media/" ("driver-" <> personId.getId) fileType contentType
  let fileUrl =
        config.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "issue"
          & T.replace "<FILE_PATH>" filePath
  _ <- fork "S3 Put Issue Media File" $ S3.put (T.unpack filePath) mediaFile
  createMediaEntry fileUrl fileType
  where
    validateContentType = do
      case fileType of
        S3.Audio | reqContentType == "audio/wave" -> pure "wav"
        S3.Audio | reqContentType == "audio/mpeg" -> pure "mp3"
        S3.Audio | reqContentType == "audio/mp4" -> pure "mp4"
        S3.Image | reqContentType == "image/png" -> pure "png"
        S3.Image | reqContentType == "image/jpeg" -> pure "jpg"
        _ -> throwError $ FileFormatNotSupported reqContentType

fetchMedia :: (HasField "s3Env" r (S3.S3Env m), MonadReader r m, BeamFlow m r) => (Id Person, Id Merchant) -> Text -> m Text
fetchMedia _personId filePath =
  S3.get $ T.unpack filePath

createIssueReport ::
  ( EsqDBReplicaFlow m r,
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
  category <- CQIC.findById categoryId identifier >>= fromMaybeM (IssueCategoryDoNotExist categoryId.getId)
  mbOption <- forM optionId \justOptionId -> do
    CQIO.findByIdAndCategoryId justOptionId categoryId identifier >>= fromMaybeM (IssueOptionInvalid justOptionId.getId categoryId.getId)
  mbRide <- forM rideId \justRideId -> do
    B.runInReplica (issueHandle.findRideById justRideId merchantId) >>= fromMaybeM (RideNotFound justRideId.getId)
  uploadedMediaFiles <- forM mediaFiles $ \mediaFile ->
    CQMF.findById mediaFile identifier >>= fromMaybeM (FileDoNotExist mediaFile.getId)
  let mediaFileUrls = map (.url) uploadedMediaFiles
  language <- getLanguage personId mbLanguage issueHandle
  issueConfig <- CQI.findIssueConfig identifier >>= fromMaybeM (InternalError "IssueConfigNotFound")
  let shouldCreateTicket = isNothing createTicket || fromJust createTicket
      onCreateIssueMsgs = if shouldCreateTicket then issueConfig.onCreateIssueMsgs else []
  issueMessageTranslationList <- mapM (\messageId -> CQIM.findByIdAndLanguage messageId language identifier) onCreateIssueMsgs
  now <- getCurrentTime
  person <- issueHandle.findPersonById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let messages = mkIssueMessageList $ sequence issueMessageTranslationList
      chats_ = fromMaybe [] chats
      updatedChats = updateChats chats_ shouldCreateTicket messages uploadedMediaFiles now
      mocId = maybe person.merchantOperatingCityId (.merchantOperatingCityId) mbRide
  issueReport <- mkIssueReport mocId updatedChats shouldCreateTicket now
  _ <- QIR.create issueReport
  when shouldCreateTicket $ do
    config <- issueHandle.findMerchantConfig merchantId mocId personId
    ticket <- buildTicket issueReport category mbOption mbRide person merchantId mocId config mediaFileUrls now issueHandle identifier
    ticketResponse <- try @_ @SomeException (issueHandle.createTicket merchantId mocId ticket)
    case ticketResponse of
      Right ticketResponse' -> do
        QIR.updateTicketId issueReport.id ticketResponse'.ticketId
      Left err -> do
        logTagInfo "Create Ticket API failed - " $ show err
  pure $ Common.IssueReportRes {issueReportId = issueReport.id, issueReportShortId = issueReport.shortId, messages}
  where
    mkIssueReport mocId updatedChats shouldCreateTicket now = do
      id <- generateGUID
      shortId <- generateShortId
      pure $
        D.IssueReport
          { id,
            shortId = Just shortId,
            personId,
            driverId = if identifier == CUSTOMER then Nothing else Just personId,
            rideId = rideId,
            merchantOperatingCityId = Just mocId,
            optionId = optionId,
            categoryId = categoryId,
            mediaFiles = mediaFiles,
            assignee = Nothing,
            status = if shouldCreateTicket then OPEN else NOT_APPLICABLE,
            deleted = False,
            ticketId = Nothing,
            createdAt = now,
            updatedAt = now,
            description,
            chats = updatedChats
          }

    buildTicket :: (EsqDBReplicaFlow m r, EncFlow m r, BeamFlow m r) => D.IssueReport -> D.IssueCategory -> Maybe D.IssueOption -> Maybe Ride -> Person -> Id Merchant -> Id MerchantOperatingCity -> MerchantConfig -> [Text] -> UTCTime -> ServiceHandle m -> Identifier -> m TIT.CreateTicketReq
    buildTicket issue category mbOption mbRide person merchId moCityId merchantCfg mediaFileUrls now issueServiceHandle identifier_ = do
      info <- buildRideInfo merchId moCityId now issueServiceHandle mbRide
      phoneNumber <- mapM decrypt person.mobileNumber
      return $
        TIT.CreateTicketReq
          { category = category.category,
            subCategory = (.option) <$> mbOption,
            disposition = merchantCfg.kaptureDisposition,
            queue = merchantCfg.kaptureQueue,
            issueId = Just issue.id.getId,
            issueDescription = description,
            mediaFiles = Just mediaFileUrls,
            name = Just $ fromMaybe "" person.firstName <> " " <> fromMaybe "" person.lastName,
            phoneNo = phoneNumber,
            personId = person.id.getId,
            classification = castIdentifierToClassification identifier_,
            rideDescription = Just info
          }

    buildRideInfo :: (EsqDBReplicaFlow m r, BeamFlow m r) => Id Merchant -> Id MerchantOperatingCity -> UTCTime -> ServiceHandle m -> Maybe Ride -> m TIT.RideInfo
    buildRideInfo mId moCityId now issueServiceHandle mbRide = do
      res <- maybe (pure Nothing) ((\rId -> Just <$> issueServiceHandle.getRideInfo mId moCityId rId) . (.id)) mbRide
      moCity <-
        issueServiceHandle.findMOCityById moCityId
          >>= fromMaybeM (MerchantOperatingCityNotFound $ "MerchantOpCityId - " <> show moCityId)
      return
        TIT.RideInfo
          { rideShortId = maybe "" (.shortId.getShortId) mbRide,
            rideCity = show moCity.city,
            customerName = (.customerName) =<< res,
            customerPhoneNo = (.customerPhoneNo) <$> res,
            driverName = (.driverName) <$> res,
            driverPhoneNo = (.driverPhoneNo) =<< res,
            vehicleNo = maybe "" (.vehicleNo) res,
            vehicleCategory = (.vehicleVariant) =<< res,
            vehicleServiceTier = (.vehicleServiceTier) =<< res,
            status = maybe "" (show . (.bookingStatus)) res,
            rideCreatedAt = maybe now (.createdAt) mbRide,
            pickupLocation = mkLocation ((.customerPickupLocation) <$> res),
            dropLocation = mkLocation . (.customerDropLocation) <$> res,
            fare = (.actualFare) =<< res
          }

    mkLocation :: Maybe Common.LocationAPIEntity -> TIT.Location
    mkLocation mbLocAPIEnt =
      TIT.Location
        { lat = maybe 0.00 (.lat) mbLocAPIEnt,
          lon = maybe 0.00 (.lon) mbLocAPIEnt,
          street = (.street) =<< mbLocAPIEnt,
          city = (.city) =<< mbLocAPIEnt,
          state = (.state) =<< mbLocAPIEnt,
          country = (.country) =<< mbLocAPIEnt,
          building = (.building) =<< mbLocAPIEnt,
          areaCode = (.areaCode) =<< mbLocAPIEnt,
          area = (.area) =<< mbLocAPIEnt
        }

    updateChats :: [Chat] -> Bool -> [Common.Message] -> [D.MediaFile] -> UTCTime -> [Chat]
    updateChats issueChats shouldCreateTicket messages mediaFiles_ now =
      let issueDescriptionChats = [mkIssueChat IssueDescription "" now]
          issueMediaFileChats = map (\mediaFile -> mkIssueChat MediaFile mediaFile.id.getId now) mediaFiles_
          chatsWithDescAndMediaFiles =
            if shouldCreateTicket || not (null issueChats)
              then issueChats ++ issueDescriptionChats ++ issueMediaFileChats
              else issueChats
       in chatsWithDescAndMediaFiles
            ++ ( if not $ null issueChats
                   then map (\message -> mkIssueChat IssueMessage message.id.getId now) messages
                   else []
               )

    castIdentifierToClassification :: Identifier -> TIT.Classification
    castIdentifierToClassification = \case
      DRIVER -> TIT.DRIVER
      CUSTOMER -> TIT.CUSTOMER

issueInfo ::
  ( EsqDBReplicaFlow m r,
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
  issueChats <- recreateIssueChats issueReport language identifier
  issueOptions <-
    if null issueReport.chats
      then pure []
      else CQIO.findAllByMessageAndLanguage (Id (last issueReport.chats).chatId) language identifier
  pure $
    Common.IssueInfoRes
      { issueReportId = issueReport.id,
        issueReportShortId = issueReport.shortId,
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

updateIssueOption ::
  BeamFlow m r =>
  Id D.IssueReport ->
  (Id Person, Id Merchant) ->
  Common.IssueUpdateReq ->
  Identifier ->
  m APISuccess
updateIssueOption issueReportId (_, _) Common.IssueUpdateReq {..} identifier = do
  void $ QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  void $ CQIO.findByIdAndCategoryId optionId categoryId identifier >>= fromMaybeM (IssueOptionInvalid optionId.getId categoryId.getId)
  _ <- QIR.updateOption issueReportId optionId
  pure Success

deleteIssue ::
  ( EsqDBReplicaFlow m r,
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
    EncFlow m r,
    BeamFlow m r
  ) =>
  (Id Person, Id Merchant, Id MerchantOperatingCity) ->
  Id D.IssueReport ->
  Maybe Language ->
  Common.IssueStatusUpdateReq ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueStatusUpdateRes
updateIssueStatus (personId, merchantId, merchantOpCityId) issueReportId mbLanguage Common.IssueStatusUpdateReq {..} issueHandle identifier = do
  language <- getLanguage personId mbLanguage issueHandle
  issueReport <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  case status of
    CLOSED -> do
      QIR.updateStatusAssignee issueReport.id (Just status) issueReport.assignee
      updateTicketStatus issueReport TIT.CL merchantId merchantOpCityId issueHandle "Closed by person"
      pure $
        Common.IssueStatusUpdateRes
          { messages = []
          }
    REOPENED -> do
      QIR.updateStatusAssignee issueReport.id (Just status) issueReport.assignee
      updateTicketStatus issueReport TIT.CRS merchantId merchantOpCityId issueHandle "Ticket reopened"
      issueConfig <- CQI.findIssueConfig identifier >>= fromMaybeM (InternalError "IssueConfigNotFound")
      issueMessageTranslation <- mapM (\messageId -> CQIM.findByIdAndLanguage messageId language identifier) issueConfig.onIssueReopenMsgs
      let issueMessages = mkIssueMessageList $ sequence issueMessageTranslation
      now <- getCurrentTime
      let updatedChats = issueReport.chats ++ map (\message -> mkIssueChat IssueMessage message.id.getId now) issueMessages
      QIR.updateChats issueReportId updatedChats
      pure $
        Common.IssueStatusUpdateRes
          { messages = issueMessages
          }
    _ -> throwError $ InternalError "Cannot Update Issue : Incorrect Status Provided"

updateTicketStatus ::
  ( EncFlow m r,
    BeamFlow m r
  ) =>
  D.IssueReport ->
  TIT.SubStatus ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  ServiceHandle m ->
  Text ->
  m ()
updateTicketStatus issueReport status merchantId merchantOperatingCityId issueHandle comment =
  case issueReport.ticketId of
    Nothing -> return ()
    Just ticketId -> do
      ticketResponse <-
        try @_ @SomeException
          (issueHandle.updateTicket merchantId merchantOperatingCityId (TIT.UpdateTicketReq comment ticketId status))
      case ticketResponse of
        Left err -> logTagInfo "Update Ticket API failed - " $ show err
        Right _ -> return ()

mkIssueMessageList ::
  Maybe [(D.IssueMessage, Maybe D.IssueTranslation)] -> [Common.Message]
mkIssueMessageList mbList = case mbList of
  Nothing -> []
  Just list ->
    map
      ( \(issueMessage, issueTranslation) ->
          Common.Message
            { id = issueMessage.id,
              message = fromMaybe issueMessage.message $ issueTranslation <&> (.translation),
              label = fromMaybe "" issueMessage.label
            }
      )
      list

mkIssueOptionList ::
  (D.IssueOption, Maybe D.IssueTranslation) -> Common.IssueOptionRes
mkIssueOptionList (issueOption, issueTranslation) =
  Common.IssueOptionRes
    { issueOptionId = issueOption.id,
      label = fromMaybe (issueOption.option & T.toUpper & T.replace " " "_") issueOption.label,
      option = fromMaybe issueOption.option $ issueTranslation <&> (.translation)
    }

recreateIssueChats :: BeamFlow m r => D.IssueReport -> Language -> Identifier -> m [ChatDetail]
recreateIssueChats issueReport language identifier =
  mapM
    ( \item -> case item.chatType of
        IssueMessage -> do
          mbIssueMessageTranslation <- CQIM.findByIdAndLanguage (Id item.chatId) language identifier
          let content = mkIssueMessage <$> mbIssueMessageTranslation
              label = (.label) . fst =<< mbIssueMessageTranslation
          pure $ mkChatDetail item.chatId item.timestamp Text BOT content label
        IssueOption -> do
          mbIssueOptionTranslation <- CQIO.findByIdAndLanguage (Id item.chatId) language identifier
          let content = mkIssueOption <$> mbIssueOptionTranslation
              label = (.label) . fst =<< mbIssueOptionTranslation
          pure $ mkChatDetail item.chatId item.timestamp Text USER content label
        IssueDescription -> pure $ mkChatDetail item.chatId item.timestamp Text USER (Just issueReport.description) Nothing
        MediaFile -> do
          mediaFile <- CQMF.findById (Id item.chatId) identifier >>= fromMaybeM (FileDoNotExist item.chatId)
          pure $ mkChatDetail item.chatId item.timestamp (mediaTypeToMessageType mediaFile._type) USER (Just mediaFile.url) Nothing
    )
    issueReport.chats
  where
    mediaTypeToMessageType :: S3.FileType -> MessageType
    mediaTypeToMessageType = \case
      S3.Audio -> Audio
      S3.Image -> Image
      _ -> Text

    mkChatDetail id timestamp chatType sender content label =
      Common.ChatDetail {..}

    mkIssueMessage :: (D.IssueMessage, Maybe D.IssueTranslation) -> Text
    mkIssueMessage (issueMessage, issueOptionTranslation) =
      fromMaybe issueMessage.message $ (.translation) <$> issueOptionTranslation

mkIssueChat :: ChatType -> Text -> UTCTime -> Chat
mkIssueChat chatType chatId timestamp =
  Chat {..}

mkIssueOption :: (D.IssueOption, Maybe D.IssueTranslation) -> Text
mkIssueOption (issueOption, issueOptionTranslation) =
  fromMaybe issueOption.option $ (.translation) <$> issueOptionTranslation

mkMediaFiles :: [D.MediaFile] -> [Common.MediaFile_]
mkMediaFiles =
  foldr'
    ( \mediaFile mediaFileList -> do
        case mediaFile._type of
          S3.Audio -> Common.MediaFile_ S3.Audio mediaFile.url : mediaFileList
          S3.Image -> Common.MediaFile_ S3.Image mediaFile.url : mediaFileList
          _ -> mediaFileList
    )
    []
