{-# LANGUAGE ScopedTypeVariables #-}

module IssueManagement.Domain.Action.Dashboard.Issue where

import qualified AWS.S3 as S3
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.List as DL
import qualified Data.Text as T hiding (count, map)
import qualified EulerHS.Language as L
import EulerHS.Prelude (withFile)
import EulerHS.Types (base64Encode)
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (..))
import IssueManagement.Common
import qualified IssueManagement.Common.Dashboard.Issue as Common
import IssueManagement.Domain.Action.UI.Issue (ServiceHandle)
import qualified IssueManagement.Domain.Action.UI.Issue as UIR
import qualified IssueManagement.Domain.Types.Issue.Comment as DC
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as DIC
import qualified IssueManagement.Domain.Types.Issue.IssueChat as DICT
import qualified IssueManagement.Domain.Types.Issue.IssueConfig as DICFG
import qualified IssueManagement.Domain.Types.Issue.IssueMessage as DIM
import IssueManagement.Domain.Types.Issue.IssueOption
import qualified IssueManagement.Domain.Types.Issue.IssueOption as DIO
import qualified IssueManagement.Domain.Types.Issue.IssueReport as DIR
import qualified IssueManagement.Domain.Types.Issue.IssueTranslation as DIT
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueCategory as CQIC
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueConfig as CQI
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueMessage as CQIM
import qualified IssueManagement.Storage.CachedQueries.Issue.IssueOption as CQIO
import qualified IssueManagement.Storage.CachedQueries.MediaFile as CQMF
import qualified IssueManagement.Storage.Queries.Issue.Comment as QC
import qualified IssueManagement.Storage.Queries.Issue.IssueCategory as QIC
import qualified IssueManagement.Storage.Queries.Issue.IssueChat as QICT
import qualified IssueManagement.Storage.Queries.Issue.IssueMessage as QIM
import qualified IssueManagement.Storage.Queries.Issue.IssueOption as QIO
import qualified IssueManagement.Storage.Queries.Issue.IssueReport as QIR
import qualified IssueManagement.Storage.Queries.Issue.IssueTranslation as QIT
import IssueManagement.Tools.Error
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import qualified Kernel.External.Ticket.Interface.Types as TIT
import qualified Kernel.External.Ticket.Kapture.Types as Kapture
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import qualified Kernel.Types.Common as Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, generateShortId, throwError)
import Kernel.Utils.Logging

-- Temporary Solution for backward Comaptibility (Remove after 1 successfull release)
getDefaultMerchantOperatingCityId :: BeamFlow m r => ServiceHandle m -> Identifier -> m (Id MerchantOperatingCity)
getDefaultMerchantOperatingCityId issueHandle identifier =
  ( issueHandle.findMOCityByMerchantShortIdAndCity shortId (Context.City "Bangalore")
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> shortId.getShortId <> "-city-" <> show (Context.City "Bangalore"))
  )
    <&> (.id)
  where
    shortId = case identifier of
      DRIVER -> ShortId "NAMMA_YATRI_PARTNER"
      CUSTOMER -> ShortId "NAMMA_YATRI"

checkMerchantCityAccess :: BeamFlow m r => ShortId Merchant -> Context.City -> DIR.IssueReport -> Maybe Person -> ServiceHandle m -> m MerchantOperatingCity
checkMerchantCityAccess merchantShortId opCity issueReport mbPerson issueHandle = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show opCity)
  case issueReport.merchantOperatingCityId of
    Nothing -> do
      person <-
        maybe
          (issueHandle.findPersonById issueReport.personId >>= fromMaybeM (PersonDoesNotExist issueReport.personId.getId))
          return
          mbPerson
      unless (person.merchantOperatingCityId == merchantOperatingCity.id) $
        throwError $ IssueReportDoesNotExist issueReport.id.getId
    Just moCityId -> do
      unless (moCityId == merchantOperatingCity.id) $
        throwError $ IssueReportDoesNotExist issueReport.id.getId
  return merchantOperatingCity

issueCategoryList :: BeamFlow m r => ShortId Merchant -> Context.City -> ServiceHandle m -> Identifier -> m Common.IssueCategoryListRes
issueCategoryList merchantShortId city issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  issueCategoryTranslationList <- CQIC.findAllActiveByMerchantOpCityIdAndLanguage merchantOperatingCity.id ENGLISH identifier
  pure $ Common.IssueCategoryListRes {categories = mkIssueCategory <$> issueCategoryTranslationList}
  where
    mkIssueCategory :: (DIC.IssueCategory, Maybe DIT.IssueTranslation) -> Common.IssueCategoryRes
    mkIssueCategory (issueCategory, issueTranslation) =
      Common.IssueCategoryRes
        { issueCategoryId = cast issueCategory.id,
          label = issueCategory.category & T.toUpper & T.replace " " "_",
          category = fromMaybe issueCategory.category $ issueTranslation <&> (.translation),
          logoUrl = issueCategory.logoUrl,
          categoryType = issueCategory.categoryType,
          isRideRequired = issueCategory.isRideRequired,
          isTicketRequired = issueCategory.isTicketRequired,
          maxAllowedRideAge = issueCategory.maxAllowedRideAge,
          allowedRideStatuses = issueCategory.allowedRideStatuses
        }

-- Helper function to safely create or get issue chat, preventing race conditions
safeCreateOrGetIssueChat ::
  BeamFlow m r =>
  Text ->
  Maybe (Id Ride) ->
  Id Person ->
  [Text] ->
  [Text] ->
  Maybe (Id DIR.IssueReport) ->
  m ()
safeCreateOrGetIssueChat ticketId mbRideId personId chats mediaFiles mbIssueReportId = do
  -- Double-check pattern to prevent race conditions
  mbExistingChat <- B.runInMasterDbAndRedis $ QICT.findByTicketId ticketId
  case mbExistingChat of
    Just existingChat -> do
      -- Update existing chat
      let updatedChats = existingChat.chats ++ chats
      let updatedMediaFiles = existingChat.mediaFiles ++ mediaFiles
      QICT.updateChats ticketId updatedChats updatedMediaFiles
    Nothing -> do
      -- Create new chat only if it still doesn't exist
      issueChat <- createNewIssueChat ticketId mbRideId personId chats mediaFiles mbIssueReportId
      -- Use exception handling to catch duplicate key violations
      result <- tryCreateIssueChat issueChat
      case result of
        Left _ -> do
          -- If creation failed (likely due to duplicate), update existing
          mbRetryChat <- B.runInMasterDbAndRedis $ QICT.findByTicketId ticketId
          case mbRetryChat of
            Just retryChat -> do
              let updatedChats = retryChat.chats ++ chats
              let updatedMediaFiles = retryChat.mediaFiles ++ mediaFiles
              QICT.updateChats ticketId updatedChats updatedMediaFiles
            Nothing -> throwError $ InternalError "Failed to create or find issue chat"
        Right _ -> pure ()
  where
    createNewIssueChat tId mbRide pId chatList mediaFilesList issueReportId = do
      now <- getCurrentTime
      issueChatId <- generateGUID
      pure $
        DICT.IssueChat
          { id = issueChatId,
            ticketId = tId,
            rideId = mbRide,
            personId = pId,
            chats = chatList,
            mediaFiles = mediaFilesList,
            kaptureData = Nothing,
            issueReportId,
            createdAt = now,
            updatedAt = now
          }

    tryCreateIssueChat issueChat = do
      withTryCatch "create:tryCreateIssueChat" (QICT.create issueChat)

-- Helper function for ticketStatusCallBack to safely handle Kapture data
safeCreateOrUpdateIssueChatWithKapture ::
  BeamFlow m r =>
  Text ->
  Maybe (Id Ride) ->
  Id Person ->
  Maybe [Kapture.ChatMessage] ->
  Maybe (Id DIR.IssueReport) ->
  m ()
safeCreateOrUpdateIssueChatWithKapture ticketId mbRideId personId kaptureData mbIssueReportId = do
  -- Double-check pattern to prevent race conditions
  mbExistingChat <- B.runInMasterDbAndRedis $ QICT.findByTicketId ticketId
  case mbExistingChat of
    Just _ -> do
      -- Update existing chat with Kapture data
      QICT.updateChatsWithKaptureData ticketId [] [] kaptureData
    Nothing -> do
      -- Create new chat only if it still doesn't exist
      now <- getCurrentTime
      issueChatId <- generateGUID
      let newIssueChat =
            DICT.IssueChat
              { id = issueChatId,
                ticketId = ticketId,
                rideId = mbRideId,
                personId = personId,
                chats = [],
                mediaFiles = [],
                kaptureData = kaptureData,
                issueReportId = mbIssueReportId,
                createdAt = now,
                updatedAt = now
              }
      -- Use exception handling to catch duplicate key violations
      result <- withTryCatch "create:ticketStatusCallBack" (QICT.create newIssueChat)
      case result of
        Left _ -> do
          -- If creation failed (likely due to duplicate), update existing
          mbRetryChat <- B.runInMasterDbAndRedis $ QICT.findByTicketId ticketId
          case mbRetryChat of
            Just _ -> QICT.updateChatsWithKaptureData ticketId [] [] kaptureData
            Nothing -> throwError $ InternalError "Failed to create or find issue chat for Kapture data"
        Right _ -> pure ()

createIssueReportV2 ::
  ( BeamFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EncFlow m r,
    TryException m
  ) =>
  ShortId Merchant ->
  Context.City ->
  Common.IssueReportReqV2 ->
  ServiceHandle m ->
  Identifier ->
  m APISuccess
createIssueReportV2 _merchantShortId _city Common.IssueReportReqV2 {..} issueHandle identifier = do
  Redis.withWaitOnLockRedisWithExpiry (issueTicketExecLockKey ticketId) 10 30 $ do
    when (identifier == DRIVER) $ do
      throwError $ InvalidRequest "Driver cannot create issue report v2"
    person <- issueHandle.findPersonById personId >>= fromMaybeM (PersonNotFound personId.getId)
    mbRide <- forM rideId \justRideId -> do
      B.runInReplica (issueHandle.findRideById justRideId person.merchantId) >>= fromMaybeM (RideNotFound justRideId.getId)
    let mocId = maybe person.merchantOperatingCityId (.merchantOperatingCityId) mbRide
    config <- issueHandle.findMerchantConfig person.merchantId mocId (Just person.id)
    UIR.processIssueReportTypeActions (person.id, person.merchantId) issueReportType mbRide (Just config) True identifier issueHandle
    mbIssueReport <- B.runInMasterDbAndRedis $ QIR.findByTicketId ticketId
    issueReport <- makeIssueReport mocId person
    when (isNothing mbIssueReport) $ QIR.create issueReport
    -- Use safe create function to prevent race conditions
    safeCreateOrGetIssueChat ticketId rideId personId chats mediaFiles (Just issueReport.id)
  pure Success
  where
    makeIssueReport mocId person = do
      id <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      pure $
        DIR.IssueReport
          { shortId = Just shortId,
            driverId = if identifier == CUSTOMER then Nothing else Just personId,
            merchantOperatingCityId = Just mocId,
            optionId = Nothing,
            categoryId = Nothing,
            mediaFiles = [],
            assignee = Nothing,
            status = OPEN,
            deleted = False,
            ticketId = Just ticketId,
            createdAt = now,
            updatedAt = now,
            description = "",
            chats = [],
            merchantId = Just (person.merchantId),
            reopenedCount = 0,
            becknIssueId = Nothing,
            ..
          }

issueList ::
  forall m r.
  ( BeamFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EncFlow m r
  ) =>
  ShortId Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe IssueStatus ->
  Maybe (Id DIC.IssueCategory) ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe (ShortId Ride) ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueReportListResponse
issueList merchantShortId opCity mbLimit mbOffset mbStatus mbCategoryId mbCategoryName mbAssignee mbMobileCountryCode mbPhoneNumber mbRideShortId mbDescriptionSearch mbFromDate mbToDate issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show opCity)
  mbCategoryIdFromName <- case (mbCategoryName, mbCategoryId) of
    (Just categoryName, Nothing) -> do
      category <- B.runInReplica $ QIC.findByNameAndMerchantOpCityId categoryName (cast merchantOperatingCity.id)
      pure (fmap (.id) category)
    _ -> pure mbCategoryId
  mbPerson <- forM mbPhoneNumber $ \phoneNumber -> do
    let mobileCountryCode = maybe "+91" (\code -> "+" <> T.strip code) mbMobileCountryCode
    numHash <- getDbHash phoneNumber
    let merchantId = merchantOperatingCity.merchantId
    B.runInReplica $
      issueHandle.findByMobileNumberAndMerchantId mobileCountryCode numHash merchantId
        >>= fromMaybeM (PersonWithPhoneNotFound phoneNumber)
  mbRide <- maybe (pure Nothing) (issueHandle.findRideByRideShortId merchantOperatingCity.merchantId) mbRideShortId
  (totalCount, issueReports) <- B.runInReplica $ QIR.findAllWithOptions mbLimit mbOffset mbStatus mbCategoryIdFromName mbAssignee ((.id) <$> mbPerson) ((.id) <$> mbRide) mbDescriptionSearch mbFromDate mbToDate (cast merchantOperatingCity.id)
  let count = length issueReports
  let summary = Common.Summary {totalCount, count}
  issues <-
    catMaybes
      <$> mapM
        ( \issueReport ->
            case issueReport.merchantOperatingCityId of
              Nothing -> do
                person <- maybe (issueHandle.findPersonById issueReport.personId >>= fromMaybeM (PersonDoesNotExist issueReport.personId.getId)) pure mbPerson
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
      catId <- issueReport.categoryId & fromMaybeM (IssueCategoryNotFound "unknown")
      category <- CQIC.findById catId identifier >>= fromMaybeM (IssueCategoryNotFound catId.getId)
      pure $
        Common.IssueReportListItem
          { issueReportId = cast issueReport.id,
            issueReportShortId = issueReport.shortId,
            personId = cast issueReport.personId,
            rideId = cast <$> issueReport.rideId,
            ticketBookingId = cast <$> issueReport.ticketBookingId,
            deleted = issueReport.deleted,
            category = category.category,
            categoryId = cast <$> issueReport.categoryId,
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
  m Common.IssueInfoDRes
issueInfo merchantShortId opCity mbIssueReportId mbIssueReportShortId issueHandle identifier = do
  issueReport <- case mbIssueReportId of
    Just iReportId -> B.runInReplica $ QIR.findById iReportId >>= fromMaybeM (IssueReportDoesNotExist iReportId.getId)
    Nothing -> case mbIssueReportShortId of
      Just iReportShortId -> B.runInReplica $ QIR.findByShortId iReportShortId >>= fromMaybeM (IssueReportDoesNotExist iReportShortId.getShortId)
      Nothing -> throwError (InvalidRequest "Either issueReportId or issueReportShortId is required")
  person <- issueHandle.findPersonById issueReport.personId >>= fromMaybeM (PersonDoesNotExist issueReport.personId.getId)
  merchantOpCity <- checkMerchantCityAccess merchantShortId opCity issueReport (Just person) issueHandle
  mkIssueInfoRes person issueReport merchantOpCity.id
  where
    mkIssueInfoRes :: (Esq.EsqDBReplicaFlow m r, EncFlow m r, BeamFlow m r) => Person -> DIR.IssueReport -> Id MerchantOperatingCity -> m Common.IssueInfoDRes
    mkIssueInfoRes person issueReport merchantOpCityId = do
      personDetail <- Just <$> mkPersonDetail person
      mediaFiles <- CQMF.findAllInForIssueReportId issueReport.mediaFiles issueReport.id identifier
      comments <- B.runInReplica (QC.findAllByIssueReportId issueReport.id)
      category <- case issueReport.categoryId of
        Nothing -> pure Nothing
        Just catId -> do
          fetchedCategory <- CQIC.findById catId identifier >>= fromMaybeM (IssueCategoryNotFound catId.getId)
          pure $ Just fetchedCategory.category
      option <- mapM (\optionId -> CQIO.findById optionId identifier >>= fromMaybeM (IssueOptionNotFound optionId.getId)) issueReport.optionId
      issueConfig <-
        CQI.findByMerchantOpCityId merchantOpCityId identifier
          >>= fromMaybeM (IssueConfigNotFound merchantOpCityId.getId)
      issueChats <- case identifier of
        DRIVER -> return Nothing
        CUSTOMER -> Just <$> UIR.recreateIssueChats issueReport issueConfig Nothing ENGLISH identifier
      pure $
        Common.IssueInfoDRes
          { issueReportId = cast issueReport.id,
            issueReportShortId = issueReport.shortId,
            personDetail,
            rideId = cast <$> issueReport.rideId,
            ticketBookingId = cast <$> issueReport.ticketBookingId,
            category = category,
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
  issueReport <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoesNotExist issueReportId.getId)
  merchantOpCity <- checkMerchantCityAccess merchantShortId opCity issueReport Nothing issueHandle
  QIR.updateStatusAssignee issueReportId req.status req.assignee
  whenJust req.assignee $ \assignee -> mkIssueAssigneeUpdateComment assignee merchantOpCity
  pure Success
  where
    mkIssueAssigneeUpdateComment assignee merchantOpCity = do
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
              merchantId = Just merchantOpCity.merchantId
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
  issueReport <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoesNotExist issueReportId.getId)
  merchantOpCity <- checkMerchantCityAccess merchantShortId opCity issueReport Nothing issueHandle
  _ <- QC.create =<< mkComment merchantOpCity
  pure Success
  where
    mkComment merchantOpCity = do
      id <- generateGUID
      now <- getCurrentTime
      pure $
        DC.Comment
          { id,
            issueReportId,
            comment = req.comment,
            authorId = cast req.userId,
            createdAt = now,
            merchantId = Just merchantOpCity.merchantId
          }

issueFetchMedia :: (HasField "s3Env" r (S3.S3Env m), MonadReader r m, BeamFlow m r) => ShortId Merchant -> Text -> m Text
issueFetchMedia _ filePath =
  S3.get $ T.unpack filePath

ticketStatusCallBack ::
  ( Esq.EsqDBReplicaFlow m r,
    BeamFlow m r
  ) =>
  A.Value ->
  ServiceHandle m ->
  Identifier ->
  m APISuccess
ticketStatusCallBack reqJson issueHandle identifier = do
  logError ("Received TicketStatusCallBackReq - " <> show reqJson)
  req <- A.decode (A.encode reqJson) & fromMaybeM (InvalidRequest "Failed to parse TicketStatusCallBackReq")
  logError ("Parsed TicketStatusCallBackReq - " <> show req)
  transformedStatus <- transformKaptureStatus req
  Redis.withWaitOnLockRedisWithExpiry (issueTicketExecLockKey req.ticketId) 10 30 $ do
    case transformedStatus of
      RESOLVED -> do
        issueReport <- B.runInMasterDbAndRedis $ QIR.findByTicketId req.ticketId >>= fromMaybeM (TicketDoesNotExist req.ticketId)
        person <- issueHandle.findPersonById issueReport.personId >>= fromMaybeM (PersonNotFound issueReport.personId.getId)
        merchantOpCityId <-
          maybe
            (return person.merchantOperatingCityId)
            return
            issueReport.merchantOperatingCityId
        issueConfig <- CQI.findByMerchantOpCityId merchantOpCityId identifier >>= fromMaybeM (IssueConfigNotFound merchantOpCityId.getId)
        let shouldUseCloseMsgs = issueReport.reopenedCount >= issueConfig.reopenCount
            selectedMsgs = if shouldUseCloseMsgs then issueConfig.onIssueCloseMsgs else issueConfig.onKaptMarkIssueResMsgs
        mbIssueMessages <- mapM (`CQIM.findById` identifier) selectedMsgs
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
        -- Get ticket details and update issueChat
        merchantId <- maybe (return person.merchantId) (const $ return person.merchantId) issueReport.merchantId
        kaptureData <- case issueHandle.kaptureGetTicket of
          Just getTicketFunc -> do
            ticketDetails <- getTicketFunc merchantId merchantOpCityId (TIT.GetTicketReq req.ticketId "all")
            return $ case ticketDetails of
              (firstTicket : _) -> Just firstTicket.conversationType.chat
              [] -> Nothing
          Nothing -> return Nothing
        -- Use safe function to handle issue chat creation/update
        safeCreateOrUpdateIssueChatWithKapture req.ticketId issueReport.rideId issueReport.personId kaptureData (Just issueReport.id)
        QIR.updateChats issueReport.id updatedChats
        QIR.updateIssueStatus req.ticketId (if shouldUseCloseMsgs then CLOSED else transformedStatus)
      PENDING_EXTERNAL -> case (req.subStatus, req.queue, issueHandle.mbSendUnattendedTicketAlert) of
        (Just "Unattended", Just "SOS", Just sendUnattendedTicketAlert) -> sendUnattendedTicketAlert req.ticketId
        _ -> do
          _issueReport <- QIR.findByTicketId req.ticketId >>= fromMaybeM (TicketDoesNotExist req.ticketId)
          QIR.updateIssueStatus req.ticketId transformedStatus
      _ -> return ()
  return Success

transformKaptureStatus :: BeamFlow m r => Common.TicketStatusCallBackReq -> m IssueStatus
transformKaptureStatus req = case req.status of
  "Complete" -> return RESOLVED
  "Pending" -> return PENDING_EXTERNAL
  _ -> throwError $ InvalidRequest ("Invalid ticket status " <> req.status <> " for ticket id " <> req.ticketId)

validateCreateIssueMessageReq :: BeamFlow m r => DIC.CategoryType -> [Common.CreateIssueMessageReq] -> Identifier -> m ()
validateCreateIssueMessageReq categoryType messages identifier = do
  case categoryType of
    DIC.FAQ ->
      mapM_
        ( \message -> do
            validateReferenceOptionAndCategoryId message
            unless (null message.options) $ throwError $ InvalidRequest "IssueMessage for an FAQ category should not contain any options."
        )
        messages
    DIC.Category -> (checkICMessages . DL.sortOn (.priority)) messages <&> (.options) >>= validateCreateIssueOptionReq categoryType identifier
  where
    checkICMessages :: BeamFlow m r => [Common.CreateIssueMessageReq] -> m Common.CreateIssueMessageReq
    checkICMessages [] = throwError $ InvalidRequest "Incomplete request"
    checkICMessages [lastMsg] = pure lastMsg
    checkICMessages (currMsg : remMsgs) = do
      unless (null currMsg.options) $ throwError $ InvalidRequest "Only the message with the highest priority value can include options."
      when (isJust currMsg.referenceOptionId || isJust currMsg.referenceOptionId) $ throwError $ InvalidRequest "An IssueCategory message should not contain reference optionId or categoryId."
      checkICMessages remMsgs

    validateReferenceOptionAndCategoryId :: BeamFlow m r => Common.CreateIssueMessageReq -> m ()
    validateReferenceOptionAndCategoryId message = do
      case (message.referenceOptionId, message.referenceCategoryId) of
        (Just _optionId, Nothing) -> throwError $ InvalidRequest "IssueMessage for an FAQ category should contain a referenceCategoryId along with referenceOptionId."
        (Just optionId, Just categoryId) -> do
          refIssueOption <- CQIO.findById optionId identifier >>= fromMaybeM (IssueOptionDoesNotExist optionId.getId)
          void $ CQIC.findById categoryId identifier >>= fromMaybeM (IssueCategoryDoesNotExist categoryId.getId)
          unless (refIssueOption.issueCategoryId == Just categoryId) $ throwError $ InvalidRequest "Reference IssueOption does not belong to reference category."
        (Nothing, Just categoryId) -> void $ CQIC.findById categoryId identifier >>= fromMaybeM (IssueCategoryDoesNotExist categoryId.getId)
        _ -> return ()

validateCreateIssueOptionReq :: BeamFlow m r => DIC.CategoryType -> Identifier -> [Common.CreateIssueOptionReq] -> m ()
validateCreateIssueOptionReq categoryType identifier =
  mapM_
    ( \option -> do
        when (length option.messages < 1) $ throwError $ InvalidRequest "IssueOptions should always contain at least one message."
        validateCreateIssueMessageReq categoryType option.messages identifier
    )

createIssueCategory ::
  BeamFlow m r =>
  ShortId Merchant ->
  Context.City ->
  Common.CreateIssueCategoryReq ->
  ServiceHandle m ->
  Identifier ->
  m Common.CreateIssueCategoryRes
createIssueCategory merchantShortId city Common.CreateIssueCategoryReq {..} issueHandle identifier = do
  validateCreateIssueMessageReq categoryType messages identifier
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  newIssueCategory <- mkIssueCategory merchantOperatingCity
  QIC.create newIssueCategory
  handleCategoryPrioriyUpdates merchantOperatingCity.id priority identifier
  upsertTranslations category Nothing merchantOperatingCity translations
  CQIC.clearAllIssueCategoryByMerchantOpCityIdAndLanguageCache merchantOperatingCity.id identifier
  mapM_
    ( \(msgReq, index) ->
        createIssueMessages merchantShortId city merchantOperatingCity newIssueCategory.id
          Nothing
          issueHandle
          (getIssueMessageType categoryType index (length messages - 1))
          msgReq
          identifier
    )
    (zip messages [0 ..])
  return $
    Common.CreateIssueCategoryRes
      { categoryId = newIssueCategory.id
      }
  where
    mkIssueCategory merchantOperatingCity = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DIC.IssueCategory
          { isActive = fromMaybe False isActive,
            createdAt = now,
            updatedAt = now,
            merchantId = merchantOperatingCity.merchantId,
            merchantOperatingCityId = merchantOperatingCity.id,
            allowedRideStatuses = allowedRideStatuses <|> Just defaultAllowedRideStatuses,
            ..
          }

createIssueMessages ::
  BeamFlow m r =>
  ShortId Merchant ->
  Context.City ->
  MerchantOperatingCity ->
  Id DIC.IssueCategory ->
  Maybe (Id DIO.IssueOption) ->
  ServiceHandle m ->
  DIM.IssueMessageType ->
  Common.CreateIssueMessageReq ->
  Identifier ->
  m APISuccess
createIssueMessages merchantShortId city merchantOperatingCity issueCategoryId mbIssueOptionId issueHandle issueMessageType Common.CreateIssueMessageReq {..} identifier = do
  newIssueMessage <- mkIssueMessage
  QIM.create newIssueMessage
  upsertTranslations message Nothing merchantOperatingCity messageTranslations
  traverse_ (\newSentence -> upsertTranslations newSentence Nothing merchantOperatingCity actionTranslations) messageAction
  traverse_ (\newSentence -> upsertTranslations newSentence Nothing merchantOperatingCity titleTranslations) messageTitle
  mapM_ ((flip $ createIssueOption merchantShortId city (Just merchantOperatingCity) issueCategoryId newIssueMessage.id issueHandle) identifier) options
  pure Success
  where
    mkIssueMessage = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DIM.IssueMessage
          { categoryId = maybe (Just issueCategoryId) (const Nothing) mbIssueOptionId,
            isActive = fromMaybe True isActive,
            optionId = mbIssueOptionId,
            messageType = issueMessageType,
            merchantId = merchantOperatingCity.merchantId,
            merchantOperatingCityId = merchantOperatingCity.id,
            createdAt = now,
            updatedAt = now,
            mediaFiles = [],
            ..
          }

updateIssueCategory :: BeamFlow m r => ShortId Merchant -> Context.City -> Id DIC.IssueCategory -> Common.UpdateIssueCategoryReq -> ServiceHandle m -> Identifier -> m APISuccess
updateIssueCategory merchantShortId city issueCategoryId req issueHandle identifier = do
  exIssueCategory <- CQIC.findById issueCategoryId identifier >>= fromMaybeM (IssueCategoryDoesNotExist issueCategoryId.getId)
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  when (exIssueCategory.merchantOperatingCityId /= merchantOperatingCity.id) $ throwError AccessDenied
  updatedIssueCategory <- mkIssueCategory exIssueCategory
  when (exIssueCategory.priority /= updatedIssueCategory.priority) $ do
    handleCategoryPrioriyUpdates merchantOperatingCity.id updatedIssueCategory.priority identifier
  CQIC.updateByPrimaryKey updatedIssueCategory
  CQIC.clearAllIssueCategoryByMerchantOpCityIdAndLanguageCache merchantOperatingCity.id identifier
  CQIC.clearIssueCategoryByIdCache exIssueCategory.id identifier
  CQIC.clearAllIssueCategoryByIdAndLanguageCache exIssueCategory.id identifier
  upsertTranslations updatedIssueCategory.category (Just exIssueCategory.category) merchantOperatingCity req.translations
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
            isActive = fromMaybe isActive req.isActive,
            isRideRequired = fromMaybe isRideRequired req.isRideRequired,
            isTicketRequired = fromMaybe isTicketRequired req.isTicketRequired,
            maxAllowedRideAge = req.maxAllowedRideAge <|> maxAllowedRideAge,
            allowedRideStatuses = req.allowedRideStatuses <|> allowedRideStatuses,
            label = req.label <|> label,
            igmCategory = req.igmCategory <|> igmCategory,
            updatedAt = now,
            ..
          }

createIssueOption ::
  BeamFlow m r =>
  ShortId Merchant ->
  Context.City ->
  Maybe MerchantOperatingCity ->
  Id DIC.IssueCategory ->
  Id DIM.IssueMessage ->
  ServiceHandle m ->
  Common.CreateIssueOptionReq ->
  Identifier ->
  m Common.CreateIssueOptionRes
createIssueOption merchantShortId city mbMerchantOpCity issueCategoryId issueMessageId issueHandle req@Common.CreateIssueOptionReq {..} identifier = do
  validateCreateIssueOptionReq DIC.Category identifier [req]
  parentIssueCategory <-
    CQIC.findById issueCategoryId identifier
      >>= fromMaybeM (IssueCategoryDoesNotExist issueCategoryId.getId)
  merchantOperatingCity <-
    maybe
      ( issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
          >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
      )
      return
      mbMerchantOpCity
  when (isNothing mbMerchantOpCity) $ do
    validateMessageCategoryAssociation issueMessageId merchantOperatingCity.id parentIssueCategory identifier
    handleOptionPriorityUpdates (Just issueMessageId.getId) priority identifier
  newIssueOption <- mkIssueOption merchantOperatingCity
  QIO.create newIssueOption
  CQIO.clearAllIssueOptionByMessageAndLanguageCache issueMessageId identifier
  upsertTranslations option Nothing merchantOperatingCity translations
  mapM_
    ( \(msgReq, index) ->
        createIssueMessages
          merchantShortId
          city
          merchantOperatingCity
          issueCategoryId
          (Just newIssueOption.id)
          issueHandle
          (getIssueMessageType parentIssueCategory.categoryType index (length messages - 1))
          msgReq
          identifier
    )
    (zip messages [0 ..])
  pure $
    Common.CreateIssueOptionRes
      { optionId = newIssueOption.id
      }
  where
    mkIssueOption merchantOperatingCity = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DIO.IssueOption
          { createdAt = now,
            updatedAt = now,
            issueCategoryId = Just issueCategoryId,
            issueMessageId = Just issueMessageId.getId,
            isActive = fromMaybe False isActive,
            restrictedVariants = fromMaybe [] restrictedVariants,
            restrictedRideStatuses = fromMaybe [] restrictedRideStatuses,
            showOnlyWhenUserBlocked = fromMaybe False showOnlyWhenUserBlocked,
            merchantId = merchantOperatingCity.merchantId,
            merchantOperatingCityId = merchantOperatingCity.id,
            igmSubCategory = igmSubCategory,
            mandatoryUploads = mandatoryUploads,
            ..
          }

updateIssueOption :: BeamFlow m r => ShortId Merchant -> Context.City -> Id DIO.IssueOption -> Common.UpdateIssueOptionReq -> ServiceHandle m -> Identifier -> m APISuccess
updateIssueOption merchantShortId city issueOptionId req issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  case (req.issueCategoryId, req.issueMessageId) of
    (Just catId, Just msgId) -> do
      parentIssueCategory <- CQIC.findById catId identifier >>= fromMaybeM (IssueCategoryDoesNotExist catId.getId)
      validateMessageCategoryAssociation msgId merchantOperatingCity.id parentIssueCategory identifier
    (Nothing, Nothing) -> return ()
    _ -> throwError $ InvalidRequest "Both categoryId and messageId is required to move option."
  exIssueOption <- CQIO.findById issueOptionId identifier >>= fromMaybeM (IssueOptionDoesNotExist issueOptionId.getId)
  when (exIssueOption.merchantOperatingCityId /= merchantOperatingCity.id) $ throwError AccessDenied
  updatedIssueOption <- mkIssueOption exIssueOption
  QIO.updateByPrimaryKey updatedIssueOption
  when (exIssueOption.priority /= updatedIssueOption.priority) $ do
    handleOptionPriorityUpdates updatedIssueOption.issueMessageId updatedIssueOption.priority identifier
  CQIO.clearAllIssueOptionByIdAndLanguageCache exIssueOption.id identifier
  CQIO.clearIssueOptionByIdCache exIssueOption.id identifier
  maybe (return ()) ((`CQIO.clearAllIssueOptionByMessageAndLanguageCache` identifier) . Id) updatedIssueOption.issueMessageId
  when (exIssueOption.issueMessageId /= updatedIssueOption.issueMessageId) $
    maybe (return ()) ((`CQIO.clearAllIssueOptionByMessageAndLanguageCache` identifier) . Id) (exIssueOption.issueMessageId)
  upsertTranslations updatedIssueOption.option (Just exIssueOption.option) merchantOperatingCity req.translations
  return Success
  where
    mkIssueOption DIO.IssueOption {..} = do
      now <- getCurrentTime
      return $
        DIO.IssueOption
          { option = fromMaybe option req.option,
            priority = fromMaybe priority req.priority,
            isActive = fromMaybe isActive req.isActive,
            issueMessageId = (req.issueMessageId <&> (.getId)) <|> issueMessageId,
            issueCategoryId = req.issueCategoryId <|> issueCategoryId,
            restrictedVariants = fold $ req.restrictedVariants <|> Just restrictedVariants,
            restrictedRideStatuses = fold $ req.restrictedRideStatuses <|> Just restrictedRideStatuses,
            showOnlyWhenUserBlocked = fromMaybe False $ req.showOnlyWhenUserBlocked <|> Just showOnlyWhenUserBlocked,
            label = req.label <|> label,
            updatedAt = now,
            mandatoryUploads = req.mandatoryUploads <|> mandatoryUploads,
            ..
          }

upsertIssueMessage :: (BeamFlow m r, MonadTime m, MonadReader r m, HasField "s3Env" r (S3.S3Env m)) => ShortId Merchant -> Context.City -> Common.UpsertIssueMessageReq -> ServiceHandle m -> Identifier -> m Common.UpsertIssueMessageRes
upsertIssueMessage merchantShortId city req issueHandle identifier = do
  existingIssueMessage <-
    traverse
      ( \issueMessageId ->
          CQIM.findById issueMessageId identifier
            >>= fromMaybeM (IssueMessageDoesNotExist issueMessageId.getId)
      )
      req.issueMessageId
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  whenJust existingIssueMessage $ \issueMessage ->
    when (issueMessage.merchantOperatingCityId /= merchantOperatingCity.id) $ throwError AccessDenied
  updatedIssueMessage <- mkIssueMessage existingIssueMessage merchantOperatingCity issueHandle
  handleMessagePriorityUpdates existingIssueMessage updatedIssueMessage.optionId updatedIssueMessage.categoryId updatedIssueMessage.messageType updatedIssueMessage.priority identifier
  whenJust existingIssueMessage $ \extIM -> do
    clearOptionAndCategoryCache extIM
    clearUpdatedIssueMessagesCacheIfRequired extIM updatedIssueMessage
  upsertTranslations updatedIssueMessage.message ((.message) <$> existingIssueMessage) merchantOperatingCity (fromMaybe [] req.messageTranslations)
  traverse_ (\newSentence -> upsertTranslations newSentence ((.messageAction) =<< existingIssueMessage) merchantOperatingCity (fromMaybe [] req.actionTranslations)) updatedIssueMessage.messageAction
  traverse_ (\newSentence -> upsertTranslations newSentence ((.messageTitle) =<< existingIssueMessage) merchantOperatingCity (fromMaybe [] req.titleTranslations)) updatedIssueMessage.messageTitle
  if isJust existingIssueMessage
    then do
      QIM.updateByPrimaryKey updatedIssueMessage
      clearIssueMessageIdCaches updatedIssueMessage
    else do
      clearOptionAndCategoryCache updatedIssueMessage
      QIM.create updatedIssueMessage
  return $
    Common.UpsertIssueMessageRes
      { messageId = updatedIssueMessage.id
      }
  where
    mkIssueMessage :: (BeamFlow m r, Common.MonadTime m, MonadReader r m, HasField "s3Env" r (S3.S3Env m)) => Maybe DIM.IssueMessage -> MerchantOperatingCity -> ServiceHandle m -> m DIM.IssueMessage
    mkIssueMessage mbIssueMessage merchantOperatingCity iHandle = do
      (mbOptionId, mbCategoryId) <- getAndValidateOptionAndCategoryId mbIssueMessage
      mbParentCategory <- findIssueCategory mbOptionId mbCategoryId
      whenJust mbParentCategory $ \parentCategory ->
        when (parentCategory.merchantOperatingCityId /= merchantOperatingCity.id) $ throwError AccessDenied
      (referenceOptionId, referenceCategoryId) <- getAndValidateReferenceOptionAnCategoryId mbIssueMessage ((.categoryType) <$> mbParentCategory)
      id <- maybe generateGUID (return . (.id)) mbIssueMessage
      now <- getCurrentTime
      message <- fromMaybeM (InvalidRequest "Message is required field for creating a new issue message") $ req.message <|> ((.message) <$> mbIssueMessage)
      priority <- fromMaybeM (InvalidRequest "Priority is required field for creating a new issue message") $ req.priority <|> ((.priority) <$> mbIssueMessage)
      let messageType = findIssueMessageType mbIssueMessage mbParentCategory
      mediaFiles <- uploadMessageMediaFiles id (foldMap (.mediaFiles) mbIssueMessage) merchantOperatingCity messageType iHandle
      return $
        DIM.IssueMessage
          { label = req.label <|> ((.label) =<< mbIssueMessage),
            createdAt = maybe now (.createdAt) mbIssueMessage,
            updatedAt = now,
            categoryId = mbCategoryId,
            merchantId = merchantOperatingCity.merchantId,
            merchantOperatingCityId = merchantOperatingCity.id,
            optionId = mbOptionId,
            messageTitle = req.messageTitle <|> ((.messageTitle) =<< mbIssueMessage),
            messageAction = req.messageAction <|> ((.messageAction) =<< mbIssueMessage),
            isActive = fromMaybe True (req.isActive <|> (mbIssueMessage <&> (.isActive))),
            ..
          }

    clearIssueMessageIdCaches :: BeamFlow m r => DIM.IssueMessage -> m ()
    clearIssueMessageIdCaches im = do
      CQIM.clearAllIssueMessageByIdAndLanguageCache im.id identifier
      CQIM.clearIssueMessageByIdCache im.id identifier

    clearUpdatedIssueMessagesCacheIfRequired :: BeamFlow m r => DIM.IssueMessage -> DIM.IssueMessage -> m ()
    clearUpdatedIssueMessagesCacheIfRequired extIM upIM = do
      when (extIM.optionId /= upIM.optionId || extIM.categoryId /= upIM.categoryId) $
        clearOptionAndCategoryCache upIM

    clearOptionAndCategoryCache :: BeamFlow m r => DIM.IssueMessage -> m ()
    clearOptionAndCategoryCache im = do
      maybe (return ()) (`CQIM.clearAllIssueMessageByOptionIdAndLanguageCache` identifier) (im.optionId)
      maybe (return ()) (`CQIM.clearAllIssueMessageByCategoryIdAndLanguageCache` identifier) (im.categoryId)

    getAndValidateOptionAndCategoryId :: BeamFlow m r => Maybe DIM.IssueMessage -> m (Maybe (Id DIO.IssueOption), Maybe (Id DIC.IssueCategory))
    getAndValidateOptionAndCategoryId mbIm =
      case (req.optionId, req.categoryId, mbIm) of
        (Just _, Just _, _) -> throwError $ InvalidRequest "IssueMessage can only have either one of optionId or categoryId"
        (_, _, Just issueMessage) -> return (issueMessage.optionId, issueMessage.categoryId)
        (Just optionId, _, _) -> do
          void $ CQIO.findById optionId identifier >>= fromMaybeM (IssueOptionDoesNotExist optionId.getId)
          return (Just optionId, Nothing)
        (_, Just categoryId, _) -> do
          void $ CQIC.findById categoryId identifier >>= fromMaybeM (IssueCategoryDoesNotExist categoryId.getId)
          return (Nothing, Just categoryId)
        _ -> throwError $ InvalidRequest "Either categoryId or optionId required to create a message."

    findIssueCategory :: BeamFlow m r => Maybe (Id DIO.IssueOption) -> Maybe (Id DIC.IssueCategory) -> m (Maybe DIC.IssueCategory)
    findIssueCategory mbOptionId mbCategoryId = do
      categoryId <- case mbCategoryId of
        Just cId -> return (Just cId)
        Nothing -> (.issueCategoryId) <$> findIssueOption
      maybe (return Nothing) (\(catId :: Id DIC.IssueCategory) -> CQIC.findById catId identifier) categoryId
      where
        findIssueOption =
          maybe
            (throwError $ InvalidRequest "Either categoryId or optionId is required")
            (\optionId -> CQIO.findById optionId identifier >>= fromMaybeM (IssueOptionNotFound optionId.getId))
            mbOptionId

    getAndValidateReferenceOptionAnCategoryId :: BeamFlow m r => Maybe DIM.IssueMessage -> Maybe DIC.CategoryType -> m (Maybe (Id DIO.IssueOption), Maybe (Id DIC.IssueCategory))
    getAndValidateReferenceOptionAnCategoryId mbIm mbCategoryType = do
      case mbCategoryType of
        Just DIC.FAQ -> case (req.referenceOptionId, req.referenceCategoryId, mbIm) of
          (Just _optionId, Nothing, _) -> throwError $ InvalidRequest "IssueMessage should contain a referenceCategoryId along with referenceOptionId."
          (Just optionId, Just categoryId, _) -> do
            refIssueOption <- CQIO.findById optionId identifier >>= fromMaybeM (IssueOptionDoesNotExist optionId.getId)
            void $ CQIC.findById categoryId identifier >>= fromMaybeM (IssueCategoryDoesNotExist categoryId.getId)
            unless (refIssueOption.issueCategoryId == Just categoryId) $ throwError $ InvalidRequest "Reference IssueOption does not belong to reference category."
            return (Just optionId, Just categoryId)
          (Nothing, Just categoryId, _) -> do
            void $ CQIC.findById categoryId identifier >>= fromMaybeM (IssueCategoryDoesNotExist categoryId.getId)
            return (Nothing, Just categoryId)
          (Nothing, Nothing, Just issueMessage) -> return (issueMessage.referenceOptionId, issueMessage.referenceCategoryId)
          _ -> return (Nothing, Nothing)
        _ -> do
          when (isJust req.referenceOptionId || isJust req.referenceCategoryId) $ throwError $ InvalidRequest "An IssueCategory message should not contain reference optionId or categoryId."
          return (Nothing, Nothing)

    findIssueMessageType :: Maybe DIM.IssueMessage -> Maybe DIC.IssueCategory -> DIM.IssueMessageType
    findIssueMessageType mbIssueMessage mbParentCategory = case mbIssueMessage of
      Just message -> message.messageType
      Nothing -> maybe DIM.Terminal (bool DIM.Intermediate DIM.FAQ . ((== DIC.FAQ) . (.categoryType))) mbParentCategory

    uploadMessageMediaFiles :: (BeamFlow m r, Common.MonadTime m, MonadReader r m, HasField "s3Env" r (S3.S3Env m)) => Id DIM.IssueMessage -> [Id DMF.MediaFile] -> MerchantOperatingCity -> DIM.IssueMessageType -> ServiceHandle m -> m [Id DMF.MediaFile]
    uploadMessageMediaFiles issueMessageId existingMediaFiles merchantOpCity messageType iHandle = do
      case messageType of
        DIM.FAQ -> do
          config <- iHandle.findMerchantConfig merchantOpCity.merchantId merchantOpCity.id Nothing
          mediaFileIds <-
            mapM
              ( \fileData -> do
                  contentType <- validateContentType fileData.reqContentType
                  fileSize <- L.runIO $ withFile fileData.file ReadMode hFileSize
                  when (fileSize > fromIntegral config.mediaFileSizeUpperLimit) $
                    throwError $ FileSizeExceededError (show fileSize)
                  mediaFile <- L.runIO $ base64Encode <$> BS.readFile fileData.file
                  filePath <- S3.createFilePath "issue-media/faqMedia/" ("messageId-" <> issueMessageId.getId) S3.Image contentType
                  let fileUrl =
                        config.mediaFileUrlPattern
                          & T.replace "<DOMAIN>" "issue"
                          & T.replace "<FILE_PATH>" filePath
                  _ <- fork "S3 Put Issue Media File" $ S3.put (T.unpack filePath) mediaFile
                  UIR.createMediaEntry fileUrl S3.Image filePath <&> (.fileId)
              )
              (fromMaybe [] req.mediaFiles)
          return $ bool (existingMediaFiles <> mediaFileIds) mediaFileIds (fromMaybe False req.deleteExistingFiles)
        _ -> return []

    validateContentType :: BeamFlow m r => Text -> m Text
    validateContentType contType =
      case contType of
        "image/png" -> pure "png"
        "image/jpeg" -> pure "jpg"
        _ -> throwError $ FileFormatNotSupported contType

upsertTranslations :: BeamFlow m r => Text -> Maybe Text -> MerchantOperatingCity -> [Translation] -> m ()
upsertTranslations newSentence mbOldSentence merchantOpCity =
  mapM_
    ( \translation -> do
        extTranslation <- QIT.findIssueTranslationByLanguageAndSentence translation.language $ fromMaybe newSentence mbOldSentence
        updatedTranslation <- mkIssueTranslation extTranslation translation
        if isJust extTranslation then QIT.updateByPrimaryKey updatedTranslation else QIT.create updatedTranslation
    )
  where
    mkIssueTranslation :: BeamFlow m r => Maybe DIT.IssueTranslation -> Translation -> m DIT.IssueTranslation
    mkIssueTranslation mbTranslation translation = do
      id <- maybe generateGUID (return . (.id)) mbTranslation
      now <- getCurrentTime
      return $
        DIT.IssueTranslation
          { translation = translation.translation,
            language = translation.language,
            sentence = newSentence,
            merchantId = merchantOpCity.merchantId,
            createdAt = maybe now (.createdAt) mbTranslation,
            updatedAt = now,
            ..
          }

validateMessageCategoryAssociation :: BeamFlow m r => Id DIM.IssueMessage -> Id MerchantOperatingCity -> DIC.IssueCategory -> Identifier -> m ()
validateMessageCategoryAssociation issueMessageId merchantOperatingCityId parentIssueCategory identifier = do
  parentIssueMessage <- CQIM.findById issueMessageId identifier >>= fromMaybeM (IssueMessageDoesNotExist issueMessageId.getId)
  when
    ( parentIssueCategory.merchantOperatingCityId /= merchantOperatingCityId
        || parentIssueMessage.merchantOperatingCityId /= merchantOperatingCityId
    )
    $ throwError AccessDenied
  when (parentIssueMessage.messageType /= DIM.Terminal) $ throwError $ InvalidRequest "Parent message has incorrect messageType."
  mbParentMessageCategoryId <-
    maybe
      (maybe (return Nothing) findCategoryId parentIssueMessage.optionId)
      (return . Just)
      parentIssueMessage.categoryId
  whenJust mbParentMessageCategoryId $ \categoryId ->
    when (categoryId /= parentIssueCategory.id) $ throwError $ InvalidRequest "Parent Message does not belong to the Parent category."
  where
    findCategoryId :: BeamFlow m r => Id DIO.IssueOption -> m (Maybe (Id DIC.IssueCategory))
    findCategoryId issueOptionId =
      CQIO.findById issueOptionId identifier
        >>= fromMaybeM
          (IssueOptionNotFound issueOptionId.getId)
        <&> (.issueCategoryId)

handleCategoryPrioriyUpdates :: BeamFlow m r => Id MerchantOperatingCity -> Int -> Identifier -> m ()
handleCategoryPrioriyUpdates merchantOpCityId priority identifier = do
  allCategories <- CQIC.findAllActiveByMerchantOpCityIdAndLanguage merchantOpCityId ENGLISH identifier
  let matchingPriority = find (\(category, _) -> category.priority == priority) allCategories
  whenJust matchingPriority $ \_ ->
    mapM_
      ( \(category, _) ->
          when (category.priority >= priority) $ do
            QIC.updatePriority category.id (category.priority + 1)
            CQIC.clearAllIssueCategoryByIdAndLanguageCache category.id identifier
            CQIC.clearIssueCategoryByIdCache category.id identifier
      )
      allCategories

handleOptionPriorityUpdates :: BeamFlow m r => Maybe Text -> Int -> Identifier -> m ()
handleOptionPriorityUpdates mbIssueMessageId priority identifier = do
  whenJust mbIssueMessageId $ \issueMessageId -> do
      allOptions <- CQIO.findAllActiveByMessageAndLanguage (Id issueMessageId) ENGLISH identifier
      let matchingPriority = find (\(option, _) -> option.priority == priority) allOptions
      whenJust matchingPriority $ \_ ->
        mapM_
          ( \(option, _) ->
              when (option.priority >= priority) $ do
                QIO.updatePriority option.id (option.priority + 1)
                CQIO.clearAllIssueOptionByIdAndLanguageCache option.id identifier
                CQIO.clearIssueOptionByIdCache option.id identifier
          )
          allOptions

handleMessagePriorityUpdates :: BeamFlow m r => Maybe DIM.IssueMessage -> Maybe (Id DIO.IssueOption) -> Maybe (Id DIC.IssueCategory) -> DIM.IssueMessageType -> Int -> Identifier -> m ()
handleMessagePriorityUpdates mbOldIssueMessage mbIssueOptionId mbIssueCategoryId messageType priority identifier = do
  issueMessages <- case mbIssueOptionId of
    Just optionId -> CQIM.findAllActiveByOptionIdAndLanguage optionId ENGLISH identifier
    Nothing -> case mbIssueCategoryId of
      Just categoryId -> CQIM.findAllActiveByCategoryIdAndLanguage categoryId ENGLISH identifier
      Nothing -> return []
  let maxPriority = maximum (map (\(message, _, _) -> message.priority) issueMessages)
  when (messageType /= DIM.FAQ && not (null issueMessages) && priority > maxPriority && messageType /= DIM.Terminal) $
    throwError $ InvalidRequest "Priority is greater than the priority of the Terminal IssueMessage."
  case mbOldIssueMessage of
    Just oldMessage -> when (oldMessage.priority /= priority) $ updatePriorities issueMessages
    Nothing -> updatePriorities issueMessages
  where
    updatePriorities :: BeamFlow m r => [(DIM.IssueMessage, DIT.DetailedTranslation, [Text])] -> m ()
    updatePriorities messages = do
      let matchingPriority = find (\(message, _, _) -> message.priority == priority) messages
      whenJust matchingPriority $ \_ ->
        mapM_
          ( \(message, _, _) ->
              when (message.priority >= priority) $ do
                QIM.updatePriority (message.id) (message.priority + 1)
                CQIM.clearAllIssueMessageByIdAndLanguageCache message.id identifier
                CQIM.clearIssueMessageByIdCache message.id identifier
          )
          messages

getIssueMessageType :: DIC.CategoryType -> Int -> Int -> DIM.IssueMessageType
getIssueMessageType DIC.FAQ _ _ = DIM.FAQ
getIssueMessageType DIC.Category currentIndex lastIndex
  | currentIndex == lastIndex = DIM.Terminal
  | otherwise = DIM.Intermediate

defaultAllowedRideStatuses :: [RideStatus]
defaultAllowedRideStatuses = [R_CANCELLED, R_COMPLETED]

issueTicketExecLockKey :: Text -> Text
issueTicketExecLockKey ticketId = "IssueTicketExec:TicketId-" <> ticketId

-----------------------------------------------------------
-- Get Category Detail API ---------------------------------

getCategoryDetail ::
  ( BeamFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  ShortId Merchant ->
  Context.City ->
  Id DIC.IssueCategory ->
  Maybe Language ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueCategoryDetailRes
getCategoryDetail merchantShortId city categoryId mbLanguage issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  let language = fromMaybe ENGLISH mbLanguage
  issueCategory <- CQIC.findById categoryId identifier >>= fromMaybeM (IssueCategoryNotFound categoryId.getId)
  when (issueCategory.merchantOperatingCityId /= merchantOperatingCity.id) $ throwError AccessDenied
  translations <- QIT.findAllBySentence issueCategory.category
  categoryMessages <- CQIM.findAllActiveByCategoryIdAndLanguage categoryId language identifier
  categoryOptions <- CQIO.findAllByCategoryAndLanguage categoryId language identifier
  messages <- mapM (mkMessageDetailRes language identifier) categoryMessages
  options <- mapM (mkOptionDetailResWithoutChildren language identifier) categoryOptions
  let categoryRes = mkIssueCategoryRes issueCategory Nothing
  pure $
    Common.IssueCategoryDetailRes
      { category = categoryRes,
        translations = mkTranslation <$> translations,
        messages = messages,
        options = options
      }
  where
    mkIssueCategoryRes :: DIC.IssueCategory -> Maybe DIT.IssueTranslation -> Common.IssueCategoryRes
    mkIssueCategoryRes issueCategory mbTranslation =
      Common.IssueCategoryRes
        { issueCategoryId = cast issueCategory.id,
          label = issueCategory.category & T.toUpper & T.replace " " "_",
          category = fromMaybe issueCategory.category $ mbTranslation <&> (.translation),
          logoUrl = issueCategory.logoUrl,
          categoryType = issueCategory.categoryType,
          isRideRequired = issueCategory.isRideRequired,
          isTicketRequired = issueCategory.isTicketRequired,
          maxAllowedRideAge = issueCategory.maxAllowedRideAge,
          allowedRideStatuses = issueCategory.allowedRideStatuses
        }

    mkTranslation :: DIT.IssueTranslation -> Translation
    mkTranslation trans =
      Translation
        { language = trans.language,
          translation = trans.translation
        }

-----------------------------------------------------------
-- Get Option Detail API -----------------------------------

getOptionDetail ::
  ( BeamFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  ShortId Merchant ->
  Context.City ->
  Id DIO.IssueOption ->
  Maybe Language ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueOptionDetailRes
getOptionDetail merchantShortId city optionId mbLanguage issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  let language = fromMaybe ENGLISH mbLanguage
  issueOption <- CQIO.findById optionId identifier >>= fromMaybeM (IssueOptionNotFound optionId.getId)
  when (issueOption.merchantOperatingCityId /= merchantOperatingCity.id) $ throwError AccessDenied
  mkOptionDetailRes language identifier (issueOption, Nothing)

-----------------------------------------------------------
-- Get Message Detail API ----------------------------------

getMessageDetail ::
  ( BeamFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  ShortId Merchant ->
  Context.City ->
  Id DIM.IssueMessage ->
  Maybe Language ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueMessageDetailRes
getMessageDetail merchantShortId city messageId mbLanguage issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  let language = fromMaybe ENGLISH mbLanguage
  (issueMessage, translation, mediaFileUrls) <- CQIM.findByIdAndLanguage messageId language identifier >>= fromMaybeM (IssueMessageDoesNotExist messageId.getId)
  when (issueMessage.merchantOperatingCityId /= merchantOperatingCity.id) $ throwError AccessDenied
  mkMessageDetailRes language identifier (issueMessage, translation, mediaFileUrls)

-----------------------------------------------------------
-- List Messages API ---------------------------------------

listMessages ::
  ( BeamFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  ShortId Merchant ->
  Context.City ->
  Maybe (Id DIC.IssueCategory) ->
  Maybe (Id DIO.IssueOption) ->
  Maybe Bool ->
  Maybe Language ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueMessageListRes
listMessages merchantShortId city mbCategoryId mbOptionId mbIsActive mbLanguage issueHandle identifier = do
  _merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  let language = fromMaybe ENGLISH mbLanguage
  issueMessages <- case (mbCategoryId, mbOptionId) of
    (Just catId, Nothing) -> CQIM.findAllActiveByCategoryIdAndLanguage catId language identifier
    (Nothing, Just optId) -> CQIM.findAllActiveByOptionIdAndLanguage optId language identifier
    (Just catId, Just _) -> CQIM.findAllActiveByCategoryIdAndLanguage catId language identifier -- prioritize categoryId
    (Nothing, Nothing) -> throwError $ InvalidRequest "Either categoryId or optionId is required to list messages"
  let filteredMessages = case mbIsActive of
        Just isActive -> filter (\(msg, _, _) -> msg.isActive == isActive) issueMessages
        Nothing -> issueMessages
  messages <- mapM (mkMessageDetailRes language identifier) filteredMessages
  pure $ Common.IssueMessageListRes {messages = messages}

-----------------------------------------------------------
-- List Options API ----------------------------------------

listOptions ::
  ( BeamFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  ShortId Merchant ->
  Context.City ->
  Maybe (Id DIC.IssueCategory) ->
  Maybe (Id DIM.IssueMessage) ->
  Maybe Bool ->
  Maybe Language ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueOptionListDRes
listOptions merchantShortId city mbCategoryId mbMessageId mbIsActive mbLanguage issueHandle identifier = do
  _merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  let language = fromMaybe ENGLISH mbLanguage
  issueOptions <- case (mbCategoryId, mbMessageId) of
    (Just catId, Nothing) -> CQIO.findAllByCategoryAndLanguage catId language identifier
    (Nothing, Just msgId) -> CQIO.findAllActiveByMessageAndLanguage msgId language identifier
    (Just _, Just msgId) -> CQIO.findAllActiveByMessageAndLanguage msgId language identifier
    (Nothing, Nothing) -> throwError $ InvalidRequest "Either categoryId or messageId is required to list options"
  let filteredOptions = case mbIsActive of
        Just isActive -> filter (\(opt, _) -> opt.isActive == isActive) issueOptions
        Nothing -> issueOptions
  options <- mapM (mkOptionDetailResWithoutChildren language identifier) filteredOptions
  pure $ Common.IssueOptionListDRes {options = options}

-----------------------------------------------------------
-- Delete (Deactivate) Category API ------------------------

deleteIssueCategory ::
  BeamFlow m r =>
  ShortId Merchant ->
  Context.City ->
  Id DIC.IssueCategory ->
  ServiceHandle m ->
  Identifier ->
  m APISuccess
deleteIssueCategory merchantShortId city categoryId issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  issueCategory <- CQIC.findById categoryId identifier >>= fromMaybeM (IssueCategoryNotFound categoryId.getId)
  when (issueCategory.merchantOperatingCityId /= merchantOperatingCity.id) $ throwError AccessDenied
  QIC.updateIsActive categoryId False
  CQIC.clearAllIssueCategoryByMerchantOpCityIdAndLanguageCache merchantOperatingCity.id identifier
  CQIC.clearIssueCategoryByIdCache categoryId identifier
  CQIC.clearAllIssueCategoryByIdAndLanguageCache categoryId identifier
  pure Success

-----------------------------------------------------------
-- Delete (Deactivate) Option API --------------------------

deleteIssueOption ::
  BeamFlow m r =>
  ShortId Merchant ->
  Context.City ->
  Id DIO.IssueOption ->
  ServiceHandle m ->
  Identifier ->
  m APISuccess
deleteIssueOption merchantShortId city optionId issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  issueOption <- CQIO.findById optionId identifier >>= fromMaybeM (IssueOptionNotFound optionId.getId)
  when (issueOption.merchantOperatingCityId /= merchantOperatingCity.id) $ throwError AccessDenied
  QIO.updateIsActive optionId False
  CQIO.clearAllIssueOptionByIdAndLanguageCache optionId identifier
  CQIO.clearIssueOptionByIdCache optionId identifier
  maybe (return ()) (\msgId -> CQIO.clearAllIssueOptionByMessageAndLanguageCache (Id msgId) identifier) issueOption.issueMessageId
  pure Success

-----------------------------------------------------------
-- Delete (Deactivate) Message API -------------------------

deleteIssueMessage ::
  BeamFlow m r =>
  ShortId Merchant ->
  Context.City ->
  Id DIM.IssueMessage ->
  ServiceHandle m ->
  Identifier ->
  m APISuccess
deleteIssueMessage merchantShortId city messageId issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  issueMessage <- CQIM.findById messageId identifier >>= fromMaybeM (IssueMessageDoesNotExist messageId.getId)
  when (issueMessage.merchantOperatingCityId /= merchantOperatingCity.id) $ throwError AccessDenied
  QIM.updateIsActive messageId False
  CQIM.clearAllIssueMessageByIdAndLanguageCache messageId identifier
  CQIM.clearIssueMessageByIdCache messageId identifier
  maybe (return ()) (`CQIM.clearAllIssueMessageByCategoryIdAndLanguageCache` identifier) issueMessage.categoryId
  maybe (return ()) (`CQIM.clearAllIssueMessageByOptionIdAndLanguageCache` identifier) issueMessage.optionId
  pure Success

-----------------------------------------------------------
-- Preview Category Flow API -------------------------------

previewCategoryFlow ::
  ( BeamFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  ShortId Merchant ->
  Context.City ->
  Id DIC.IssueCategory ->
  Maybe Language ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueCategoryFlowPreviewRes
previewCategoryFlow merchantShortId city categoryId mbLanguage issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  let language = fromMaybe ENGLISH mbLanguage
  issueCategory <- CQIC.findById categoryId identifier >>= fromMaybeM (IssueCategoryNotFound categoryId.getId)
  when (issueCategory.merchantOperatingCityId /= merchantOperatingCity.id) $ throwError AccessDenied
  categoryMessages <- CQIM.findAllActiveByCategoryIdAndLanguage categoryId language identifier
  flowNodes <- buildFlowNodes language identifier categoryMessages
  let categoryRes = mkIssueCategoryResForPreview issueCategory Nothing
  pure $
    Common.IssueCategoryFlowPreviewRes
      { category = categoryRes,
        flowNodes = flowNodes
      }
  where
    mkIssueCategoryResForPreview :: DIC.IssueCategory -> Maybe DIT.IssueTranslation -> Common.IssueCategoryRes
    mkIssueCategoryResForPreview issueCategory mbTranslation =
      Common.IssueCategoryRes
        { issueCategoryId = cast issueCategory.id,
          label = issueCategory.category & T.toUpper & T.replace " " "_",
          category = fromMaybe issueCategory.category $ mbTranslation <&> (.translation),
          logoUrl = issueCategory.logoUrl,
          categoryType = issueCategory.categoryType,
          isRideRequired = issueCategory.isRideRequired,
          isTicketRequired = issueCategory.isTicketRequired,
          maxAllowedRideAge = issueCategory.maxAllowedRideAge,
          allowedRideStatuses = issueCategory.allowedRideStatuses
        }

buildFlowNodes ::
  ( BeamFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  Language ->
  Identifier ->
  [(DIM.IssueMessage, DIT.DetailedTranslation, [Text])] ->
  m [Common.MessageFlowNode]
buildFlowNodes language identifier messages = do
  let sortedMessages = DL.sortOn (\(msg, _, _) -> msg.priority) messages
  mapM (buildMessageFlowNode language identifier) sortedMessages

buildMessageFlowNode ::
  ( BeamFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  Language ->
  Identifier ->
  (DIM.IssueMessage, DIT.DetailedTranslation, [Text]) ->
  m Common.MessageFlowNode
buildMessageFlowNode language identifier (msg, translation, _) = do
  childOptions <- CQIO.findAllActiveByMessageAndLanguage msg.id language identifier
  optionNodes <- mapM (buildOptionFlowNode language identifier) childOptions
  let messagePreview =
        Common.MessagePreview
          { messageId = cast msg.id,
            message = fromMaybe msg.message (translation.contentTranslation <&> (.translation)),
            messageTitle = (translation.titleTranslation <&> (.translation)) <|> msg.messageTitle,
            messageAction = (translation.actionTranslation <&> (.translation)) <|> msg.messageAction,
            label = msg.label,
            messageType = msg.messageType
          }
  pure $
    Common.MessageFlowNode
      { message = messagePreview,
        options = optionNodes,
        nextMessage = Nothing
      }

buildOptionFlowNode ::
  ( BeamFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  Language ->
  Identifier ->
  (DIO.IssueOption, Maybe DIT.IssueTranslation) ->
  m Common.OptionFlowNode
buildOptionFlowNode language identifier (opt, mbTranslation) = do
  optionMessages <- CQIM.findAllActiveByOptionIdAndLanguage opt.id language identifier
  childNodes <- mapM (buildMessageFlowNode language identifier) optionMessages
  pure $
    Common.OptionFlowNode
      { optionId = cast opt.id,
        option = fromMaybe opt.option $ mbTranslation <&> (.translation),
        label = opt.label,
        childNodes = childNodes
      }

-----------------------------------------------------------
-- Translation APIs ----------------------------------------

getTranslations ::
  BeamFlow m r =>
  ShortId Merchant ->
  Context.City ->
  Text ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueTranslationListRes
getTranslations merchantShortId city sentence issueHandle _identifier = do
  _merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  translations <- QIT.findAllBySentence sentence
  pure $
    Common.IssueTranslationListRes
      { translations = mkTranslationItem <$> translations
      }
  where
    mkTranslationItem :: DIT.IssueTranslation -> Common.IssueTranslationItem
    mkTranslationItem trans =
      Common.IssueTranslationItem
        { sentence = trans.sentence,
          language = trans.language,
          translation = trans.translation
        }

bulkUpsertTranslations ::
  BeamFlow m r =>
  ShortId Merchant ->
  Context.City ->
  Common.BulkUpsertTranslationsReq ->
  ServiceHandle m ->
  Identifier ->
  m APISuccess
bulkUpsertTranslations merchantShortId city req issueHandle _identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  mapM_ (upsertSingleTranslation merchantOperatingCity) req.translations
  pure Success
  where
    upsertSingleTranslation merchantOpCity item = do
      upsertTranslations item.sentence Nothing merchantOpCity [Translation item.language item.translation]

-----------------------------------------------------------
-- IssueConfig APIs ----------------------------------------

getIssueConfig ::
  BeamFlow m r =>
  ShortId Merchant ->
  Context.City ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueConfigRes
getIssueConfig merchantShortId city issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  issueConfig <- CQI.findByMerchantOpCityId merchantOperatingCity.id identifier >>= fromMaybeM (IssueConfigNotFound merchantOperatingCity.id.getId)
  pure $
    Common.IssueConfigRes
      { issueConfigId = cast issueConfig.id,
        autoMarkIssueClosedDuration = issueConfig.autoMarkIssueClosedDuration,
        onCreateIssueMsgs = cast <$> issueConfig.onCreateIssueMsgs,
        onIssueReopenMsgs = cast <$> issueConfig.onIssueReopenMsgs,
        onAutoMarkIssueClsMsgs = cast <$> issueConfig.onAutoMarkIssueClsMsgs,
        onKaptMarkIssueResMsgs = cast <$> issueConfig.onKaptMarkIssueResMsgs,
        onIssueCloseMsgs = cast <$> issueConfig.onIssueCloseMsgs,
        reopenCount = issueConfig.reopenCount,
        merchantName = issueConfig.messageTransformationConfig >>= (.merchantName),
        supportEmail = issueConfig.messageTransformationConfig >>= (.supportEmail)
      }

updateIssueConfig ::
  BeamFlow m r =>
  ShortId Merchant ->
  Context.City ->
  Common.UpdateIssueConfigReq ->
  ServiceHandle m ->
  Identifier ->
  m APISuccess
updateIssueConfig merchantShortId city req issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  issueConfig <- CQI.findByMerchantOpCityId merchantOperatingCity.id identifier >>= fromMaybeM (IssueConfigNotFound merchantOperatingCity.id.getId)
  now <- getCurrentTime
  let updatedConfig =
        issueConfig
          { DICFG.autoMarkIssueClosedDuration = fromMaybe issueConfig.autoMarkIssueClosedDuration req.autoMarkIssueClosedDuration,
            DICFG.onCreateIssueMsgs = maybe issueConfig.onCreateIssueMsgs (map cast) req.onCreateIssueMsgs,
            DICFG.onIssueReopenMsgs = maybe issueConfig.onIssueReopenMsgs (map cast) req.onIssueReopenMsgs,
            DICFG.onAutoMarkIssueClsMsgs = maybe issueConfig.onAutoMarkIssueClsMsgs (map cast) req.onAutoMarkIssueClsMsgs,
            DICFG.onKaptMarkIssueResMsgs = maybe issueConfig.onKaptMarkIssueResMsgs (map cast) req.onKaptMarkIssueResMsgs,
            DICFG.onIssueCloseMsgs = maybe issueConfig.onIssueCloseMsgs (map cast) req.onIssueCloseMsgs,
            DICFG.reopenCount = fromMaybe issueConfig.reopenCount req.reopenCount,
            DICFG.updatedAt = now
          }
  CQI.updateByPrimaryKey updatedConfig
  CQI.clearIssueConfigCache merchantOperatingCity.id identifier
  pure Success

-----------------------------------------------------------
-- Reorder APIs --------------------------------------------

reorderCategories ::
  BeamFlow m r =>
  ShortId Merchant ->
  Context.City ->
  Common.ReorderIssueCategoryReq ->
  ServiceHandle m ->
  Identifier ->
  m APISuccess
reorderCategories merchantShortId city req issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  mapM_
    ( \(catId, newPriority) -> do
        issueCategory <- CQIC.findById catId identifier >>= fromMaybeM (IssueCategoryNotFound catId.getId)
        when (issueCategory.merchantOperatingCityId /= merchantOperatingCity.id) $ throwError AccessDenied
        QIC.updatePriority catId newPriority
        CQIC.clearIssueCategoryByIdCache catId identifier
        CQIC.clearAllIssueCategoryByIdAndLanguageCache catId identifier
    )
    req.categoryOrder
  CQIC.clearAllIssueCategoryByMerchantOpCityIdAndLanguageCache merchantOperatingCity.id identifier
  pure Success

reorderOptions ::
  BeamFlow m r =>
  ShortId Merchant ->
  Context.City ->
  Common.ReorderIssueOptionReq ->
  ServiceHandle m ->
  Identifier ->
  m APISuccess
reorderOptions merchantShortId city req issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  mapM_
    ( \(optId, newPriority) -> do
        issueOption <- CQIO.findById optId identifier >>= fromMaybeM (IssueOptionNotFound optId.getId)
        when (issueOption.merchantOperatingCityId /= merchantOperatingCity.id) $ throwError AccessDenied
        QIO.updatePriority optId newPriority
        CQIO.clearIssueOptionByIdCache optId identifier
        CQIO.clearAllIssueOptionByIdAndLanguageCache optId identifier
    )
    req.optionOrder
  pure Success

reorderMessages ::
  BeamFlow m r =>
  ShortId Merchant ->
  Context.City ->
  Common.ReorderIssueMessageReq ->
  ServiceHandle m ->
  Identifier ->
  m APISuccess
reorderMessages merchantShortId city req issueHandle identifier = do
  merchantOperatingCity <-
    issueHandle.findMOCityByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> merchantShortId.getShortId <> "-city-" <> show city)
  mapM_
    ( \(msgId, newPriority) -> do
        issueMessage <- CQIM.findById msgId identifier >>= fromMaybeM (IssueMessageDoesNotExist msgId.getId)
        when (issueMessage.merchantOperatingCityId /= merchantOperatingCity.id) $ throwError AccessDenied
        QIM.updatePriority msgId newPriority
        CQIM.clearIssueMessageByIdCache msgId identifier
        CQIM.clearAllIssueMessageByIdAndLanguageCache msgId identifier
    )
    req.messageOrder
  pure Success

-----------------------------------------------------------
-- Helper Functions ----------------------------------------

mkMessageDetailRes ::
  ( BeamFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  Language ->
  Identifier ->
  (DIM.IssueMessage, DIT.DetailedTranslation, [Text]) ->
  m Common.IssueMessageDetailRes
mkMessageDetailRes language identifier (msg, translation, _mediaFileUrls) = do
  childOptions <- CQIO.findAllActiveByMessageAndLanguage msg.id language identifier
  childOptionDetails <- mapM (mkOptionDetailResWithoutChildren language identifier) childOptions
  mediaFiles <- CQMF.findAllInForIssueReportId msg.mediaFiles (cast msg.id) identifier
  pure $
    Common.IssueMessageDetailRes
      { messageId = cast msg.id,
        message = fromMaybe msg.message (translation.contentTranslation <&> (.translation)),
        messageTitle = (translation.titleTranslation <&> (.translation)) <|> msg.messageTitle,
        messageAction = (translation.actionTranslation <&> (.translation)) <|> msg.messageAction,
        label = msg.label,
        priority = msg.priority,
        messageType = msg.messageType,
        isActive = msg.isActive,
        mediaFiles = mediaFiles,
        translations =
          Common.DetailedTranslationRes
            { titleTranslation = mkTranslationList translation.titleTranslation,
              contentTranslation = mkTranslationList translation.contentTranslation,
              actionTranslation = mkTranslationList translation.actionTranslation
            },
        childOptions = childOptionDetails
      }
  where
    mkTranslationList :: Maybe DIT.IssueTranslation -> [Translation]
    mkTranslationList (Just t) = [Translation t.language t.translation]
    mkTranslationList Nothing = []

mkOptionDetailRes ::
  ( BeamFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  Language ->
  Identifier ->
  (DIO.IssueOption, Maybe DIT.IssueTranslation) ->
  m Common.IssueOptionDetailRes
mkOptionDetailRes language identifier (opt, mbTranslation) = do
  translations <- QIT.findAllBySentence opt.option
  childMessages <- CQIM.findAllActiveByOptionIdAndLanguage opt.id language identifier
  childMessageDetails <- mapM (mkMessageDetailRes language identifier) childMessages
  pure $
    Common.IssueOptionDetailRes
      { optionId = cast opt.id,
        option = fromMaybe opt.option $ mbTranslation <&> (.translation),
        label = opt.label,
        priority = opt.priority,
        isActive = opt.isActive,
        translations = mkTranslation <$> translations,
        childMessages = childMessageDetails
      }
  where
    mkTranslation :: DIT.IssueTranslation -> Translation
    mkTranslation trans = Translation trans.language trans.translation

mkOptionDetailResWithoutChildren ::
  BeamFlow m r =>
  Language ->
  Identifier ->
  (DIO.IssueOption, Maybe DIT.IssueTranslation) ->
  m Common.IssueOptionDetailRes
mkOptionDetailResWithoutChildren _language _identifier (opt, mbTranslation) = do
  translations <- QIT.findAllBySentence opt.option
  pure $
    Common.IssueOptionDetailRes
      { optionId = cast opt.id,
        option = fromMaybe opt.option $ mbTranslation <&> (.translation),
        label = opt.label,
        priority = opt.priority,
        isActive = opt.isActive,
        translations = mkTranslation <$> translations,
        childMessages = []
      }
  where
    mkTranslation :: DIT.IssueTranslation -> Translation
    mkTranslation trans = Translation trans.language trans.translation
