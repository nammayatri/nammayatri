module IssueManagement.Domain.Action.Dashboard.Issue where

import qualified AWS.S3 as S3
import qualified Data.Text as T hiding (count, map)
import IssueManagement.Common
import qualified IssueManagement.Common.Dashboard.Issue as Common
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
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, throwError)

newtype ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person -> m (Maybe Person)
  }

issueCategoryList :: (CacheFlow m r, BeamFlow m) => ShortId Merchant -> Identifier -> m Common.IssueCategoryListRes
issueCategoryList _merchantShortId identifier = do
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
  ( CacheFlow m r,
    BeamFlow m,
    Esq.EsqDBReplicaFlow m r
  ) =>
  ShortId Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe IssueStatus ->
  Maybe (Id DIC.IssueCategory) ->
  Maybe Text ->
  Identifier ->
  m Common.IssueReportListResponse
issueList _merchantShortId mbLimit mbOffset mbStatus mbCategoryId mbAssignee identifier = do
  issueReports <- B.runInReplica $ QIR.findAllWithOptions mbLimit mbOffset mbStatus mbCategoryId mbAssignee
  let count = length issueReports
  let summary = Common.Summary {totalCount = count, count}
  issues <- mapM mkIssueReport issueReports
  pure $ Common.IssueReportListResponse {issues, summary}
  where
    mkIssueReport :: (CacheFlow m r, Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r) => DIR.IssueReport -> m Common.IssueReportListItem
    mkIssueReport issueReport = do
      category <- CQIC.findById issueReport.categoryId identifier >>= fromMaybeM (IssueCategoryNotFound issueReport.categoryId.getId)
      pure $
        Common.IssueReportListItem
          { issueReportId = cast issueReport.id,
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
    CacheFlow m r,
    BeamFlow m,
    EncFlow m r
  ) =>
  ShortId Merchant ->
  Id DIR.IssueReport ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueInfoRes
issueInfo _merchantShortId issueReportId issueHandle identifier = do
  issueReport <- B.runInReplica $ QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
  mediaFiles <- CQMF.findAllInForIssueReportId issueReport.mediaFiles issueReportId identifier
  comments <- B.runInReplica (QC.findAllByIssueReportId issueReport.id)
  category <- CQIC.findById issueReport.categoryId identifier >>= fromMaybeM (IssueCategoryNotFound issueReport.categoryId.getId)
  personDetail <- mapM mkPersonDetail =<< issueHandle.findPersonById issueReport.personId
  option <- mapM (\optionId -> CQIO.findById optionId identifier >>= fromMaybeM (IssueOptionNotFound optionId.getId)) issueReport.optionId
  pure $
    Common.IssueInfoRes
      { issueReportId = cast issueReport.id,
        personDetail,
        rideId = cast <$> issueReport.rideId,
        category = category.category,
        option = option <&> (.option),
        mediaFiles = mediaFiles,
        comments = mkIssueReportComment <$> comments,
        description = issueReport.description,
        assignee = issueReport.assignee,
        status = issueReport.status,
        createdAt = issueReport.createdAt
      }
  where
    mkPersonDetail :: (Esq.EsqDBReplicaFlow m r, CacheFlow m r, Esq.EsqDBFlow m r, EncFlow m r) => Person -> m Common.PersonDetail
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
  ( CacheFlow m r,
    BeamFlow m
  ) =>
  ShortId Merchant ->
  Id DIR.IssueReport ->
  Common.IssueUpdateByUserReq ->
  m APISuccess
issueUpdate _merchantShortId issueReportId req = do
  unless (isJust req.status || isJust req.assignee) $
    throwError $ InvalidRequest "Empty request, no fields to update."
  _ <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
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
    CacheFlow m r,
    BeamFlow m
  ) =>
  ShortId Merchant ->
  Id DIR.IssueReport ->
  Common.IssueAddCommentByUserReq ->
  m APISuccess
issueAddComment _merchantShortId issueReportId req = do
  void $ QIR.findById issueReportId >>= fromMaybeM (IssueReportDoNotExist issueReportId.getId)
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

issueFetchMedia :: (HasField "s3Env" r (S3.S3Env m), MonadReader r m) => ShortId Merchant -> Text -> m Text
issueFetchMedia _ filePath =
  S3.get $ T.unpack filePath

ticketStatusCallBack ::
  ( Esq.EsqDBReplicaFlow m r,
    CacheFlow m r,
    BeamFlow m
  ) =>
  ShortId Merchant ->
  Identifier ->
  Common.TicketStatusCallBackReq ->
  m APISuccess
ticketStatusCallBack _ identifier req = do
  issueReport <- QIR.findByTicketId req.ticketId >>= fromMaybeM (TicketDoesNotExist req.ticketId)
  issueConfig <- CQI.findIssueConfig identifier >>= fromMaybeM (InternalError "IssueConfigNotFound")
  mbIssueMessages <- mapM (`CQIM.findById` identifier) issueConfig.onKaptMarkIssueAwtMsgs
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
