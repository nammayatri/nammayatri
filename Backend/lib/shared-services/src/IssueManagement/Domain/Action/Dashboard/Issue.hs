module IssueManagement.Domain.Action.Dashboard.Issue where

import qualified AWS.S3 as S3
import qualified Data.Text as T hiding (count, map)
import IssueManagement.Common
import qualified IssueManagement.Common.Dashboard.Issue as Common
import qualified IssueManagement.Domain.Action.UI.Issue as UIR
import qualified IssueManagement.Domain.Types.Issue.Comment as DC
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as DIC
import qualified IssueManagement.Domain.Types.Issue.IssueReport as DIR
import qualified IssueManagement.Domain.Types.Issue.IssueTranslation as DIT
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueCategory as CQIC
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueConfig as CQI
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueMessage as CQIM
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueOption as CQIO
import qualified IssueManagement.Storage.CachedQueries.MediaFile as CQMF
import qualified IssueManagement.Storage.Queries.Issue.Comment as QC
import qualified IssueManagement.Storage.Queries.Issue.IssueReport as QIR
import IssueManagement.Tools.Error
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, throwError)

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person -> m (Maybe Person),
    findByMerchantShortIdAndCity :: ShortId Merchant -> Context.City -> m (Maybe MerchantOperatingCity)
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
        CUSTOMER -> Just <$> UIR.mkIssueChats issueReport ENGLISH identifier
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
  m APISuccess
issueUpdate merchantShortId opCity issueReportId issueHandle req = do
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
              createdAt = now
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
  m APISuccess
issueAddComment merchantShortId opCity issueReportId issueHandle req = do
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
            createdAt = now
          }

issueFetchMedia :: (HasField "s3Env" r (S3.S3Env m), MonadReader r m, BeamFlow m r) => ShortId Merchant -> Text -> m Text
issueFetchMedia _ filePath =
  S3.get $ T.unpack filePath

ticketStatusCallBack ::
  ( Esq.EsqDBReplicaFlow m r,
    BeamFlow m r
  ) =>
  ShortId Merchant ->
  Context.City ->
  Identifier ->
  Common.TicketStatusCallBackReq ->
  m APISuccess
ticketStatusCallBack _merchantShortId _opCity identifier req = do
  issueReport <- QIR.findByTicketId req.ticketId >>= fromMaybeM (TicketDoesNotExist req.ticketId)
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
  QIR.updateIssueStatus req.ticketId req.status
  return Success
