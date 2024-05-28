module IssueManagement.Domain.Action.Dashboard.Issue where

import qualified AWS.S3 as S3
import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.Text as T hiding (count, map)
import qualified EulerHS.Language as L
import EulerHS.Prelude (withFile)
import EulerHS.Types (base64Encode)
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (..))
import IssueManagement.Common
import qualified IssueManagement.Common.Dashboard.Issue as Common
import qualified IssueManagement.Domain.Action.UI.Issue as UIR
import qualified IssueManagement.Domain.Types.Issue.Comment as DC
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as DIC
import qualified IssueManagement.Domain.Types.Issue.IssueMessage as DIM
import qualified IssueManagement.Domain.Types.Issue.IssueOption as DIO
import qualified IssueManagement.Domain.Types.Issue.IssueReport as DIR
import qualified IssueManagement.Domain.Types.Issue.IssueTranslation as DIT
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueCategory as CQIC
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueConfig as CQI
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueMessage as CQIM
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueOption as CQIO
import qualified IssueManagement.Storage.CachedQueries.MediaFile as CQMF
import qualified IssueManagement.Storage.Queries.Issue.Comment as QC
import qualified IssueManagement.Storage.Queries.Issue.IssueCategory as QIC
import qualified IssueManagement.Storage.Queries.Issue.IssueMessage as QIM
import qualified IssueManagement.Storage.Queries.Issue.IssueOption as QIO
import qualified IssueManagement.Storage.Queries.Issue.IssueReport as QIR
import qualified IssueManagement.Storage.Queries.Issue.IssueTranslation as QIT
import IssueManagement.Tools.Error
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, throwError)

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person -> m (Maybe Person),
    findByMerchantShortIdAndCity :: ShortId Merchant -> Context.City -> m (Maybe MerchantOperatingCity),
    findMerchantConfig :: Id Merchant -> Id MerchantOperatingCity -> Maybe (Id Person) -> m MerchantConfig
  }

checkMerchantCityAccess :: (BeamFlow m r) => ShortId Merchant -> Context.City -> DIR.IssueReport -> Maybe Person -> ServiceHandle m -> m ()
checkMerchantCityAccess merchantShortId opCity issueReport mbPerson issueHandle = do
  merchantOperatingCity <-
    issueHandle.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show opCity)
  case issueReport.merchantOperatingCityId of
    Nothing -> do
      person <-
        maybe
          (issueHandle.findPersonById issueReport.personId >>= fromMaybeM (PersonDoesNotExist issueReport.personId.getId))
          return
          mbPerson
      unless (person.merchantOperatingCityId == merchantOperatingCity.id) $
        throwError $ IssueReportDoNotExist issueReport.id.getId
    Just moCityId ->
      unless (moCityId == merchantOperatingCity.id) $
        throwError $ IssueReportDoNotExist issueReport.id.getId

issueCategoryList :: BeamFlow m r => ShortId Merchant -> Context.City -> Identifier -> m Common.IssueCategoryListRes
issueCategoryList _merchantShortId _opCity identifier = do
  issueCategoryTranslationList <- CQIC.findAllByLanguage ENGLISH identifier
  pure $ Common.IssueCategoryListRes {categories = mkIssueCategory <$> issueCategoryTranslationList}
  where
    mkIssueCategory :: (DIC.IssueCategory, Maybe DIT.IssueTranslation) -> Common.IssueCategoryRes
    mkIssueCategory (issueCategory, issueTranslation) =
      Common.IssueCategoryRes
        { issueCategoryId = cast issueCategory.id,
          label = issueCategory.category & T.toUpper & T.replace " " "_",
          category = fromMaybe issueCategory.category $ issueTranslation <&> (.translation)
        }

issueList ::
  ( BeamFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  ShortId Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe IssueStatus ->
  Maybe (Id DIC.IssueCategory) ->
  Maybe Text ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueReportListResponse
issueList merchantShortId opCity mbLimit mbOffset mbStatus mbCategoryId mbAssignee issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show opCity)
  issueReports <- B.runInReplica $ QIR.findAllWithOptions mbLimit mbOffset mbStatus mbCategoryId mbAssignee
  let count = length issueReports
  let summary = Common.Summary {totalCount = count, count}
  issues <-
    catMaybes
      <$> mapM
        ( \issueReport ->
            case issueReport.merchantOperatingCityId of
              Nothing -> do
                person <- issueHandle.findPersonById issueReport.personId >>= fromMaybeM (PersonDoesNotExist issueReport.personId.getId)
                if person.merchantOperatingCityId /= merchantOperatingCity.id
                  then return Nothing
                  else Just <$> mkIssueReport issueReport
              Just moCityId ->
                if moCityId /= merchantOperatingCity.id
                  then return Nothing
                  else Just <$> mkIssueReport issueReport
        )
        issueReports
  pure $ Common.IssueReportListResponse {issues, summary}
  where
    mkIssueReport :: (Esq.EsqDBReplicaFlow m r, BeamFlow m r) => DIR.IssueReport -> m Common.IssueReportListItem
    mkIssueReport issueReport = do
      category <- CQIC.findById issueReport.categoryId identifier >>= fromMaybeM (IssueCategoryNotFound issueReport.categoryId.getId)
      pure $
        Common.IssueReportListItem
          { issueReportId = cast issueReport.id,
            issueReportShortId = issueReport.shortId,
            personId = cast issueReport.personId,
            rideId = cast <$> issueReport.rideId,
            deleted = issueReport.deleted,
            category = category.category,
            assignee = issueReport.assignee,
            status = issueReport.status,
            createdAt = issueReport.createdAt
          }

issueInfo ::
  ( Esq.EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r
  ) =>
  ShortId Merchant ->
  Context.City ->
  Maybe (Id DIR.IssueReport) ->
  Maybe (ShortId DIR.IssueReport) ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueInfoRes
issueInfo merchantShortId opCity mbIssueReportId mbIssueReportShortId issueHandle identifier = do
  issueReport <- case mbIssueReportId of
    Just iReportId -> B.runInReplica $ QIR.findById iReportId >>= fromMaybeM (IssueReportDoNotExist iReportId.getId)
    Nothing -> case mbIssueReportShortId of
      Just iReportShortId -> B.runInReplica $ QIR.findByShortId iReportShortId >>= fromMaybeM (IssueReportDoNotExist iReportShortId.getShortId)
      Nothing -> throwError (InvalidRequest "Either issueReportId or issueReportShortId is required")
  person <- issueHandle.findPersonById issueReport.personId >>= fromMaybeM (PersonDoesNotExist issueReport.personId.getId)
  checkMerchantCityAccess merchantShortId opCity issueReport (Just person) issueHandle
  mkIssueInfoRes person issueReport
  where
    mkIssueInfoRes :: (Esq.EsqDBReplicaFlow m r, EncFlow m r, BeamFlow m r) => Person -> DIR.IssueReport -> m Common.IssueInfoRes
    mkIssueInfoRes person issueReport = do
      personDetail <- Just <$> mkPersonDetail person
      mediaFiles <- CQMF.findAllInForIssueReportId issueReport.mediaFiles issueReport.id identifier
      comments <- B.runInReplica (QC.findAllByIssueReportId issueReport.id)
      category <- CQIC.findById issueReport.categoryId identifier >>= fromMaybeM (IssueCategoryNotFound issueReport.categoryId.getId)
      option <- mapM (\optionId -> CQIO.findById optionId identifier >>= fromMaybeM (IssueOptionNotFound optionId.getId)) issueReport.optionId
      issueChats <- case identifier of
        DRIVER -> return Nothing
        CUSTOMER -> Just <$> UIR.recreateIssueChats issueReport ENGLISH identifier
      pure $
        Common.IssueInfoRes
          { issueReportId = cast issueReport.id,
            issueReportShortId = issueReport.shortId,
            personDetail,
            rideId = cast <$> issueReport.rideId,
            category = category.category,
            chats = issueChats,
            option = option <&> (.option),
            mediaFiles = mediaFiles,
            comments = mkIssueReportComment <$> comments,
            description = issueReport.description,
            assignee = issueReport.assignee,
            status = issueReport.status,
            createdAt = issueReport.createdAt
          }

    mkPersonDetail :: (Esq.EsqDBReplicaFlow m r, EncFlow m r, BeamFlow m r) => Person -> m Common.PersonDetail
    mkPersonDetail personDetail = do
      mobileNumber <- traverse decrypt personDetail.mobileNumber
      pure $
        Common.PersonDetail
          { personId = cast personDetail.id,
            firstName = personDetail.firstName,
            middleName = personDetail.middleName,
            lastName = personDetail.lastName,
            mobileNumber
          }
    mkAuthorDetails :: Id Person -> Common.AuthorDetail
    mkAuthorDetails authorId =
      Common.AuthorDetail
        { authorId = cast authorId,
          firstName = Nothing,
          lastName = Nothing
        }
    mkIssueReportComment :: DC.Comment -> Common.IssueReportCommentItem
    mkIssueReportComment comment =
      Common.IssueReportCommentItem
        { comment = comment.comment,
          authorDetail = mkAuthorDetails $ cast comment.authorId,
          timestamp = comment.createdAt
        }

issueUpdate ::
  BeamFlow m r =>
  ShortId Merchant ->
  Context.City ->
  Id DIR.IssueReport ->
  ServiceHandle m ->
  Common.IssueUpdateByUserReq ->
  Id Merchant ->
  m APISuccess
issueUpdate merchantShortId opCity issueReportId issueHandle req merchantId = do
  unless (isJust req.status || isJust req.assignee) $
    throwError $ InvalidRequest "Empty request, no fields to update."
  issueReport <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  checkMerchantCityAccess merchantShortId opCity issueReport Nothing issueHandle
  QIR.updateStatusAssignee issueReportId req.status req.assignee
  whenJust req.assignee mkIssueAssigneeUpdateComment
  pure Success
  where
    mkIssueAssigneeUpdateComment assignee = do
      id <- generateGUID
      now <- getCurrentTime
      void $
        QC.create $
          DC.Comment
            { id,
              issueReportId,
              authorId = cast req.userId,
              comment = "Assignee Updated : " <> assignee,
              createdAt = now,
              merchantId = Just merchantId
            }

issueAddComment ::
  ( Esq.EsqDBReplicaFlow m r,
    BeamFlow m r
  ) =>
  ShortId Merchant ->
  Context.City ->
  Id DIR.IssueReport ->
  ServiceHandle m ->
  Common.IssueAddCommentByUserReq ->
  Id Merchant ->
  m APISuccess
issueAddComment merchantShortId opCity issueReportId issueHandle req merchantId = do
  issueReport <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  checkMerchantCityAccess merchantShortId opCity issueReport Nothing issueHandle
  _ <- QC.create =<< mkComment
  pure Success
  where
    mkComment = do
      id <- generateGUID
      now <- getCurrentTime
      pure $
        DC.Comment
          { id,
            issueReportId,
            comment = req.comment,
            authorId = cast req.userId,
            createdAt = now,
            merchantId = Just merchantId
          }

issueFetchMedia :: (HasField "s3Env" r (S3.S3Env m), MonadReader r m, BeamFlow m r) => ShortId Merchant -> Text -> m Text
issueFetchMedia _ filePath =
  S3.get $ T.unpack filePath

ticketStatusCallBack ::
  ( Esq.EsqDBReplicaFlow m r,
    BeamFlow m r
  ) =>
  Common.TicketStatusCallBackReq ->
  Identifier ->
  m APISuccess
ticketStatusCallBack req identifier = do
  issueReport <- QIR.findByTicketId req.ticketId >>= fromMaybeM (TicketDoesNotExist req.ticketId)
  transformedStatus <- transformKaptureStatus req
  when (transformedStatus == RESOLVED) $ do
    issueConfig <- CQI.findIssueConfig identifier >>= fromMaybeM (InternalError "IssueConfigNotFound")
    mbIssueMessages <- mapM (`CQIM.findById` identifier) issueConfig.onKaptMarkIssueResMsgs
    let issueMessageIds = mapMaybe ((.id) <$>) mbIssueMessages
    now <- getCurrentTime
    let updatedChats =
          issueReport.chats
            ++ map
              ( \id ->
                  Chat
                    { chatType = IssueMessage,
                      chatId = id.getId,
                      timestamp = now
                    }
              )
              issueMessageIds
    QIR.updateChats issueReport.id updatedChats
  QIR.updateIssueStatus req.ticketId transformedStatus
  return Success

transformKaptureStatus :: BeamFlow m r => Common.TicketStatusCallBackReq -> m IssueStatus
transformKaptureStatus req = case req.status of
  "Complete" -> return RESOLVED
  "Pending" -> return PENDING_EXTERNAL
  _ -> throwError $ InvalidRequest ("Invalid ticket status " <> req.status <> " for ticket id " <> req.ticketId)

------------------------------------ HANDLE DELETION OF CACHE FOR ALL APIS ---------------------------------------

validateCreateIssueMessageReq :: BeamFlow m r => DIC.CategoryType -> [Common.CreateIssueMessageReq] -> m ()
validateCreateIssueMessageReq categoryType messages = do
  case categoryType of
    DIC.FAQ -> mapM_ (\message -> unless (null message.options) $ throwError $ InvalidRequest "IssueMessage for an FAQ category should not contain any options.") messages
    DIC.Category -> checkICMessages messages <&> (.options) >>= validateCreateIssueOptionReq categoryType
  where
    checkICMessages :: BeamFlow m r => [Common.CreateIssueMessageReq] -> m Common.CreateIssueMessageReq
    checkICMessages [] = throwError $ InvalidRequest "Incomplete request"
    checkICMessages [lastMsg] = pure lastMsg
    checkICMessages (currMsg : remMsgs) = do
      unless (null currMsg.options) $ throwError $ InvalidRequest "An intermediate IssueMessage cannot contain any IssueOptions."
      checkICMessages remMsgs

validateCreateIssueOptionReq :: BeamFlow m r => DIC.CategoryType -> [Common.CreateIssueOptionReq] -> m ()
validateCreateIssueOptionReq categoryType =
  mapM_
    ( \option -> do
        when (length option.messages < 1) $ throwError $ InvalidRequest "IssueOptions should always contain at least one message."
        validateCreateIssueMessageReq categoryType option.messages
    )

createIssueCategory :: BeamFlow m r => ShortId Merchant -> Context.City -> Common.CreateIssueCategoryReq -> Identifier -> m APISuccess
createIssueCategory merchantShortId city Common.CreateIssueCategoryReq {..} identifier = do
  validateCreateIssueMessageReq categoryType messages
  newIssueCategory <- mkIssueCategory
  QIC.create newIssueCategory
  upsertTranslations category Nothing translations
  mapM_ ((flip $ createIssueMessages merchantShortId city newIssueCategory.id Nothing) identifier) messages
  return Success
  where
    mkIssueCategory = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DIC.IssueCategory
          { isActive = fromMaybe False isActive,
            createdAt = now,
            updatedAt = now,
            ..
          }

createIssueMessages :: BeamFlow m r => ShortId Merchant -> Context.City -> Id DIC.IssueCategory -> Maybe (Id DIO.IssueOption) -> Common.CreateIssueMessageReq -> Identifier -> m APISuccess
createIssueMessages merchantShortId city issueCategoryId mbIssueOptionId Common.CreateIssueMessageReq {..} identifier = do
  newIssueMessage <- mkIssueMessage
  QIM.create newIssueMessage
  upsertTranslations message Nothing translations
  mapM_ ((flip $ createIssueOption merchantShortId city issueCategoryId newIssueMessage.id) identifier) options
  pure Success
  where
    mkIssueMessage = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DIM.IssueMessage
          { categoryId = maybe (Just issueCategoryId) (const Nothing) mbIssueOptionId,
            optionId = mbIssueOptionId,
            createdAt = now,
            updatedAt = now,
            ..
          }

updateIssueCategory :: BeamFlow m r => ShortId Merchant -> Context.City -> Id DIC.IssueCategory -> Common.UpdateIssueCategoryReq -> Identifier -> m APISuccess
updateIssueCategory _merchantShortId _city issueCategoryId req identifier = do
  exIssueCategory <- CQIC.findById issueCategoryId identifier >>= fromMaybeM (InvalidRequest "Cound not find an issueCategory with the provided id")
  updatedIssueCategory <- mkIssueCategory exIssueCategory
  CQIC.updateByPrimaryKey updatedIssueCategory
  upsertTranslations updatedIssueCategory.category (Just exIssueCategory.category) req.translations
  return Success
  where
    mkIssueCategory :: BeamFlow m r => DIC.IssueCategory -> m DIC.IssueCategory
    mkIssueCategory DIC.IssueCategory {..} = do
      now <- getCurrentTime
      return $
        DIC.IssueCategory
          { category = fromMaybe category req.category,
            logoUrl = fromMaybe logoUrl req.logoUrl,
            priority = fromMaybe priority req.priority,
            categoryType = fromMaybe categoryType req.categoryType,
            isActive = fromMaybe isActive req.isActive,
            updatedAt = now,
            ..
          }

createIssueOption :: BeamFlow m r => ShortId Merchant -> Context.City -> Id DIC.IssueCategory -> Id DIM.IssueMessage -> Common.CreateIssueOptionReq -> Identifier -> m APISuccess
createIssueOption merchantShortId city issueCategoryId issueMessageId req@Common.CreateIssueOptionReq {..} identifier = do
  void $ CQIC.findById issueCategoryId identifier >>= fromMaybeM (InvalidRequest "Could not find an IssueCategory with the provided categoryId.")
  void $ CQIM.findById issueMessageId identifier >>= fromMaybeM (InvalidRequest "Could not find an IssueMessage with the provided messageId.")
  validateCreateIssueOptionReq DIC.Category [req]
  newIssueOption <- mkIssueOption
  QIO.create newIssueOption
  upsertTranslations option Nothing translations
  mapM_ ((flip $ createIssueMessages merchantShortId city issueCategoryId (Just newIssueOption.id)) identifier) messages
  pure Success
  where
    mkIssueOption = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DIO.IssueOption
          { createdAt = now,
            updatedAt = now,
            issueCategoryId = Just issueCategoryId,
            issueMessageId = Just issueMessageId.getId,
            isActive = fromMaybe False isActive,
            ..
          }

-- Maybe Check if the message belongs to category?
updateIssueOption :: BeamFlow m r => ShortId Merchant -> Context.City -> Id DIO.IssueOption -> Common.UpdateIssueOptionReq -> Identifier -> m APISuccess
updateIssueOption _merchantShortId _city issueOptionId req identifier = do
  whenJust (req.issueCategoryId) $ \categoryId ->
    void $ CQIC.findById categoryId identifier >>= fromMaybeM (InvalidRequest "Could not find an IssueCategory with the provided categoryId.")
  whenJust (req.issueMessageId) $ \messageId ->
    void $ CQIM.findById (Id messageId) identifier >>= fromMaybeM (InvalidRequest "Could not find an IssueMessage with the provided messageId")
  exIssueOption <- CQIO.findById issueOptionId identifier >>= fromMaybeM (InvalidRequest "IssueOption with given id not found")
  updatedIssueOption <- mkIssueOption exIssueOption
  QIO.updateByPrimaryKey updatedIssueOption
  upsertTranslations updatedIssueOption.option (Just exIssueOption.option) req.translations
  return Success
  where
    mkIssueOption DIO.IssueOption {..} = do
      now <- getCurrentTime
      return $
        DIO.IssueOption
          { option = fromMaybe option req.option,
            priority = fromMaybe priority req.priority,
            isActive = fromMaybe isActive req.isActive,
            issueMessageId = req.issueMessageId <|> issueMessageId,
            issueCategoryId = req.issueCategoryId <|> issueCategoryId,
            label = req.label <|> label,
            updatedAt = now,
            ..
          }

upsertIssueMessage :: BeamFlow m r => ShortId Merchant -> Context.City -> Maybe (Id DIM.IssueMessage) -> Common.UpsertIssueMessageReq -> Identifier -> m APISuccess
upsertIssueMessage _merchantShortId _city mbIssueMessageId req identifier = do
  existingIssueMessage <-
    traverse
      ( \issueMessageId ->
          CQIM.findById issueMessageId identifier
            >>= fromMaybeM (InvalidRequest "Could not find an IssueMessage with the provided id.")
      )
      mbIssueMessageId
  updatedIssueMessage <- mkIssueMessage existingIssueMessage
  if isJust existingIssueMessage then QIM.updateByPrimaryKey updatedIssueMessage else QIM.create updatedIssueMessage
  return Success
  where
    mkIssueMessage mbIssueMessage = do
      (mbOptionId, mbCategoryId) <- getandValidateOptionAndCategoryId mbIssueMessage
      id <- maybe generateGUID (return . (.id)) mbIssueMessage
      now <- getCurrentTime
      message <- fromMaybeM (InvalidRequest "Message is required field for creating a new issue message") $ req.message <|> ((.message) <$> mbIssueMessage)
      priority <- fromMaybeM (InvalidRequest "Priority is required field for creating a new issue message") $ req.priority <|> ((.priority) <$> mbIssueMessage)
      return $
        DIM.IssueMessage
          { label = req.label <|> ((.label) =<< mbIssueMessage),
            createdAt = maybe now (.createdAt) mbIssueMessage,
            updatedAt = now,
            categoryId = mbCategoryId,
            optionId = mbOptionId,
            ..
          }

    getandValidateOptionAndCategoryId mbIM =
      case (req.optionId, req.categoryId, mbIM) of
        (Just _, Just _, _) -> throwError $ InvalidRequest "IssueMessage can only have either one of optionId or categoryId"
        (Just optionId, _, _) -> do
          void $ CQIO.findById optionId identifier >>= fromMaybeM (InvalidRequest "Could not find an IssueOption with the provided optionId.")
          return (Just optionId, Nothing)
        (_, Just categoryId, _) -> do
          void $ CQIC.findById categoryId identifier >>= fromMaybeM (InvalidRequest "Could not find an IssueCategory with the provided categoryId.")
          return (Nothing, Just categoryId)
        (_, _, Just issueMessage) -> return (issueMessage.optionId, issueMessage.categoryId)
        _ -> throwError $ InvalidRequest "Either categoryId or optionId required."

uploadIssueMessageMediaFiles ::
  ( BeamFlow m r,
    MonadTime m,
    MonadReader r m,
    HasField "s3Env" r (S3.S3Env m),
    EsqDBReplicaFlow m r
  ) =>
  ShortId Merchant ->
  Context.City ->
  Common.IssueMessageMediaFileUploadListReq ->
  ServiceHandle m ->
  m APISuccess
uploadIssueMessageMediaFiles merchantShortId city request issueHandle = do
  merchantOpCity <-
    issueHandle.findByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  config <- issueHandle.findMerchantConfig merchantOpCity.merchantId merchantOpCity.id Nothing
  mapM_
    ( \fileData -> do
        contentType <- validateContentType fileData.reqContentType
        fileSize <- L.runIO $ withFile fileData.file ReadMode hFileSize
        when (fileSize > fromIntegral config.mediaFileSizeUpperLimit) $
          throwError $ FileSizeExceededError (show fileSize)
        mediaFile <- L.runIO $ base64Encode <$> BS.readFile fileData.file
        filePath <- S3.createFilePath "issue-media/" ("faqMedia-messageId-" <> request.issueMessageId.getId) S3.Image contentType
        let fileUrl =
              config.mediaFileUrlPattern
                & T.replace "<DOMAIN>" "issue"
                & T.replace "<FILE_PATH>" filePath
        _ <- fork "S3 Put Issue Media File" $ S3.put (T.unpack filePath) mediaFile
        void $ UIR.createMediaEntry fileUrl S3.Image
    )
    request.mediaFiles
  return Success
  where
    validateContentType contType =
      case contType of
        "image/png" -> pure "png"
        "image/jpeg" -> pure "jpg"
        _ -> throwError $ FileFormatNotSupported contType

-- data IssueMessageMediaFileUploadListReq = IssueMessageMediaFileUploadListReq
--   { issueMessageId :: Id IssueMessage,
--     mediaFiles :: [IssueMessageMediaFileUploadReq]
--   }
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

-- data IssueMessageMediaFileUploadReq = IssueMessageMediaFileUploadReq
--   { file :: FilePath,
--     reqContentType :: Text
--   }
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

upsertTranslations :: BeamFlow m r => Text -> Maybe Text -> [Common.Translation] -> m ()
upsertTranslations newSentence mbOldSentence =
  mapM_
    ( \translation -> do
        extTranslation <- QIT.findIssueTranslationByLanguageAndSentence translation.language $ fromMaybe newSentence mbOldSentence
        updatedTranslation <- mkIssueTranslation extTranslation translation
        if isJust extTranslation then QIT.updateByPrimaryKey updatedTranslation else QIT.create updatedTranslation
    )
  where
    mkIssueTranslation :: BeamFlow m r => Maybe DIT.IssueTranslation -> Common.Translation -> m DIT.IssueTranslation
    mkIssueTranslation mbTranslation translation = do
      id <- maybe generateGUID (return . (.id)) mbTranslation
      now <- getCurrentTime
      return $
        DIT.IssueTranslation
          { translation = translation.translation,
            language = translation.language,
            sentence = newSentence,
            createdAt = maybe now (.createdAt) mbTranslation,
            updatedAt = now,
            ..
          }
