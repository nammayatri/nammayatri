module IssueManagement.Domain.Action.UI.Issue where

import qualified AWS.S3 as S3
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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
import qualified Kernel.Types.Beckn.City as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified "external" Slack.AWS.Flow as Slack
import Text.Regex.TDFA (AllTextMatches (..), getAllTextMatches, (=~))

data ServiceHandle m = ServiceHandle
  { findRideById :: Id Ride -> Id Merchant -> m (Maybe Ride),
    findByBookingId :: Id Booking -> m (Maybe Booking),
    findOneByBookingId :: Id Booking -> Id Merchant -> m (Maybe Ride),
    findByMerchantId :: Id Merchant -> m (Maybe Merchant),
    findPersonById :: Id Person -> m (Maybe Person),
    findMOCityById :: Id MerchantOperatingCity -> m (Maybe MerchantOperatingCity),
    findMOCityByMerchantShortIdAndCity :: ShortId Merchant -> Context.City -> m (Maybe MerchantOperatingCity),
    getRideInfo :: Id Merchant -> Id MerchantOperatingCity -> Id Ride -> m RideInfoRes,
    createTicket :: Id Merchant -> Id MerchantOperatingCity -> TIT.CreateTicketReq -> m TIT.CreateTicketResp,
    updateTicket :: Id Merchant -> Id MerchantOperatingCity -> TIT.UpdateTicketReq -> m TIT.UpdateTicketResp,
    findMerchantConfig :: Id Merchant -> Id MerchantOperatingCity -> Maybe (Id Person) -> m MerchantConfig,
    mbReportACIssue :: Maybe (BaseUrl -> Text -> Text -> m APISuccess), -- Deprecated
    mbReportIssue :: Maybe (BaseUrl -> Text -> Text -> IssueReportType -> m APISuccess),
    mbFindLatestBookingByPersonId :: Maybe (Id Person -> m (Maybe Booking)),
    mbFindRideByBookingId :: Maybe (Id Booking -> Id Merchant -> m (Maybe Ride)),
    mbSyncRide :: Maybe (Id Merchant -> Id Ride -> m ())
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
  (Id Person, Id Merchant, Id MerchantOperatingCity) ->
  Maybe Language ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueCategoryListRes
getIssueCategory (personId, _, merchantOpCityId) mbLanguage issueHandle identifier = do
  language <- getLanguage personId mbLanguage issueHandle
  issueCategoryTranslationList <- do
    categoriesWithTranslations <- CQIC.findAllActiveByMerchantOpCityIdAndLanguage merchantOpCityId language identifier
    case categoriesWithTranslations of
      [] -> do
        defaultMerchantOpCityId <-
          issueHandle.findMOCityByMerchantShortIdAndCity shortId Context.Bangalore
            >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-short-Id-" <> shortId.getShortId <> "-city-" <> show Context.Bangalore) <&> (.id)
        CQIC.findAllActiveByMerchantOpCityIdAndLanguage defaultMerchantOpCityId language identifier
      _ -> return categoriesWithTranslations
  pure $ Common.IssueCategoryListRes {categories = mkIssueCategory <$> issueCategoryTranslationList}
  where
    mkIssueCategory :: (D.IssueCategory, Maybe D.IssueTranslation) -> Common.IssueCategoryRes
    mkIssueCategory (issueCategory, issueTranslation) =
      Common.IssueCategoryRes
        { issueCategoryId = issueCategory.id,
          label = fromMaybe (issueCategory.category & T.toUpper & T.replace " " "_") issueCategory.label,
          category = fromMaybe issueCategory.category $ issueTranslation <&> (.translation),
          logoUrl = issueCategory.logoUrl,
          categoryType = issueCategory.categoryType,
          isRideRequired = issueCategory.isRideRequired,
          maxAllowedRideAge = issueCategory.maxAllowedRideAge,
          allowedRideStatuses = issueCategory.allowedRideStatuses
        }

    shortId = case identifier of
      DRIVER -> ShortId "NAMMA_YATRI_PARTNER"
      CUSTOMER -> ShortId "NAMMA_YATRI"

getIssueOption ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  (Id Person, Id Merchant, Id MerchantOperatingCity) ->
  Id D.IssueCategory ->
  Maybe (Id D.IssueOption) ->
  Maybe (Id D.IssueReport) ->
  Maybe (Id Ride) ->
  Maybe Language ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueOptionListRes
getIssueOption (personId, merchantId, merchantOpCityId) issueCategoryId issueOptionId issueReportId mbRideId mbLanguage issueHandle identifier = do
  category <- CQIC.findById issueCategoryId identifier >>= fromMaybeM (IssueCategoryDoesNotExist issueCategoryId.getId)
  mbIssueOption <- maybe (return Nothing) (`CQIO.findById` identifier) issueOptionId
  mbRideInfoRes <- mapM (issueHandle.getRideInfo merchantId merchantOpCityId) mbRideId
  case (mbRideInfoRes, category.maxAllowedRideAge) of
    (Just rideInfo, Just maxAllowedRideAge) -> do
      now <- utctimeToSeconds <$> getCurrentTime
      unless (utctimeToSeconds rideInfo.rideCreatedAt > (now - maxAllowedRideAge)) $ throwError $ InvalidRequest "Invalid ride selected."
    _ -> return ()
  let adjMerchantOpCityId = maybe merchantOpCityId Id ((.merchantOperatingCityId) =<< mbRideInfoRes)
  language <- getLanguage personId mbLanguage issueHandle
  issueConfig <- CQI.findByMerchantOpCityId adjMerchantOpCityId identifier >>= fromMaybeM (IssueConfigNotFound adjMerchantOpCityId.getId)
  processIssueReportTypeActions (personId, merchantId) mbIssueOption Nothing Nothing False identifier issueHandle
  issueMessageTranslationList <- case issueOptionId of
    Nothing -> CQIM.findAllActiveByCategoryIdAndLanguage issueCategoryId language identifier
    Just optionId -> CQIM.findAllActiveByOptionIdAndLanguage optionId language identifier
  let issueMessages = mkIssueMessageList (Just issueMessageTranslationList) language issueConfig mbRideInfoRes
  issueOptionTranslationList <- do
    case (issueOptionId, issueReportId) of
      (Just optionId, Just iReportId) -> do
        issueReport <- QIR.findById iReportId >>= fromMaybeM (IssueReportDoesNotExist iReportId.getId)
        now <- getCurrentTime
        let updatedChats =
              issueReport.chats ++ (mkIssueChat IssueOption optionId.getId now) :
              map (\message -> mkIssueChat IssueMessage message.id.getId now) issueMessages
        QIR.updateChats iReportId updatedChats
      _ -> return ()
    if null issueMessages
      then pure []
      else (filterOptions mbRideInfoRes category.label issueHandle) =<< CQIO.findAllActiveByMessageAndLanguage ((.id) $ last issueMessages) language identifier
  pure $
    Common.IssueOptionListRes
      { options = map (mkIssueOptionList issueConfig language mbRideInfoRes) issueOptionTranslationList,
        messages = issueMessages
      }
  where
    utctimeToSeconds :: UTCTime -> Seconds
    utctimeToSeconds utcTime = Seconds . floor $ diffUTCTime utcTime (posixSecondsToUTCTime 0)

    filterOptions :: BeamFlow m r => Maybe RideInfoRes -> Maybe Text -> ServiceHandle m -> [(D.IssueOption, Maybe D.IssueTranslation)] -> m [(D.IssueOption, Maybe D.IssueTranslation)]
    filterOptions mbRideInfoRes mbCategoryLabel iHandle optionsWithTranslations =
      filterOptionsBasedOnVariantsAndRideStatus mbRideInfoRes
        <$> filterOptionsBasedOnCategoryLabel mbCategoryLabel iHandle mbRideInfoRes optionsWithTranslations

    filterOptionsBasedOnVariantsAndRideStatus :: Maybe RideInfoRes -> [(D.IssueOption, Maybe D.IssueTranslation)] -> [(D.IssueOption, Maybe D.IssueTranslation)]
    filterOptionsBasedOnVariantsAndRideStatus mbRideInfoRes = do
      let mbRideVariant = (.vehicleVariant) =<< mbRideInfoRes
          mbRideStatus = (.rideStatus) <$> mbRideInfoRes
      filter
        ( \optionWithTranslation ->
            (isNothing mbRideVariant || maybe True (\variant -> variant `notElem` (.restrictedVariants) (fst optionWithTranslation)) mbRideVariant)
              && (isNothing mbRideStatus || maybe True (\rideStatus -> rideStatus `notElem` (.restrictedRideStatuses) (fst optionWithTranslation)) mbRideStatus)
        )

    filterOptionsBasedOnCategoryLabel :: BeamFlow m r => Maybe Text -> ServiceHandle m -> Maybe RideInfoRes -> [(D.IssueOption, Maybe D.IssueTranslation)] -> m [(D.IssueOption, Maybe D.IssueTranslation)]
    filterOptionsBasedOnCategoryLabel mbCategoryLabel iHandle mbRideInfoRes optionsWithTranslations = case mbCategoryLabel of
      Just "APP_RELATED" -> do
        person <- iHandle.findPersonById personId >>= fromMaybeM (PersonNotFound personId.getId)
        case person.blocked of
          Just True -> return $ filterBlockOptions True optionsWithTranslations
          _ -> return $ filterBlockOptions False optionsWithTranslations
      Just "PAYMENT_AND_FARE_RELATED" ->
        return $
          bool optionsWithTranslations (filter (\(option, _) -> option.label /= Just "SHOW_FARE_BREAKUP") optionsWithTranslations) (((.computedPrice) =<< mbRideInfoRes) == ((.estimatedFare) <$> mbRideInfoRes))
      _ -> return optionsWithTranslations

    filterBlockOptions :: Bool -> [(D.IssueOption, Maybe D.IssueTranslation)] -> [(D.IssueOption, Maybe D.IssueTranslation)]
    filterBlockOptions condition = filter (\optionWithTranslation -> (.showOnlyWhenUserBlocked) (fst optionWithTranslation) == condition)

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
  issueConfig <- CQI.findByMerchantOpCityId merchantOpCityId identifier >>= fromMaybeM (IssueConfigNotFound merchantOpCityId.getId)
  now <- getCurrentTime
  issues <- mapM (processIssueReport issueConfig now language issueHandle) issueReports
  return $ Common.IssueReportListRes {issues}
  where
    processIssueReport ::
      ( BeamFlow m r,
        EncFlow m r
      ) =>
      D.IssueConfig ->
      UTCTime ->
      Language ->
      ServiceHandle m ->
      D.IssueReport ->
      m Common.IssueReportListItem
    processIssueReport iConfig currTime language iHandle iReport = do
      let timeDiff = realToFrac (currTime `diffUTCTime` iReport.updatedAt) / 3600
      if iReport.status == RESOLVED && timeDiff > iConfig.autoMarkIssueClosedDuration
        then do
          QIR.updateStatusAssignee iReport.id (Just CLOSED) iReport.assignee
          updateTicketStatus iReport TIT.CL merchantId merchantOpCityId iHandle "Closed by system"
          mbIssueMessages <- mapM (`CQIM.findById` identifier) iConfig.onAutoMarkIssueClsMsgs
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
      mbIssueOption <- maybe (return Nothing) (`CQIO.findById` identifier) issueReport.optionId
      return $
        Common.IssueReportListItem
          { issueReportId = issueReport.id,
            issueReportShortId = issueReport.shortId,
            category = fromMaybe issueCategory.category $ issueCategoryTranslation <&> (.translation),
            rideId = issueReport.rideId,
            optionLabel = mbIssueOption >>= (.label),
            status = fromMaybe issueReport.status issueStatus,
            createdAt = issueReport.createdAt
          }

createMediaEntry :: BeamFlow m r => Text -> S3.FileType -> Text -> m Common.IssueMediaUploadRes
createMediaEntry url fileType filePath = do
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
            s3FilePath = Just filePath,
            createdAt = now
          }

issueMediaUpload' ::
  ( BeamFlow m r,
    MonadTime m,
    MonadReader r m,
    HasField "s3Env" r (S3.S3Env m),
    EsqDBReplicaFlow m r
  ) =>
  (Id Person, Id Merchant, Id MerchantOperatingCity) ->
  ServiceHandle m ->
  Common.IssueMediaUploadReq ->
  Text ->
  Text ->
  m Common.IssueMediaUploadRes
issueMediaUpload' (personId, merchantId, merchantOperatingCityId) issueHandle Common.IssueMediaUploadReq {..} domain identifier = do
  contentType <- validateContentType
  config <- issueHandle.findMerchantConfig merchantId merchantOperatingCityId (Just personId)
  fileSize <- L.runIO $ withFile file ReadMode hFileSize
  when (fileSize > fromIntegral config.mediaFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile file
  filePath <- S3.createFilePath (domain <> "/") identifier fileType contentType
  let fileUrl =
        config.mediaFileUrlPattern
          & T.replace "<DOMAIN>" domain
          & T.replace "<FILE_PATH>" filePath
  _ <- fork "S3 Put Issue Media File" $ S3.put (T.unpack filePath) mediaFile
  createMediaEntry fileUrl fileType filePath
  where
    validateContentType = do
      case fileType of
        S3.Audio | reqContentType == "audio/wave" -> pure "wav"
        S3.Audio | reqContentType == "audio/mpeg" -> pure "mp3"
        S3.Audio | reqContentType == "audio/mp4" -> pure "mp4"
        S3.Image | reqContentType == "image/png" -> pure "png"
        S3.Image | reqContentType == "image/jpeg" -> pure "jpg"
        _ -> throwError $ FileFormatNotSupported reqContentType

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
  config <- issueHandle.findMerchantConfig merchantId person.merchantOperatingCityId (Just personId)
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
  createMediaEntry fileUrl fileType filePath
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
    BeamFlow m r,
    HasField "sosAlertsTopicARN" r Text
  ) =>
  (Id Person, Id Merchant) ->
  Maybe Language ->
  Common.IssueReportReq ->
  ServiceHandle m ->
  Identifier ->
  Maybe Text ->
  m Common.IssueReportRes
createIssueReport (personId, merchantId) mbLanguage Common.IssueReportReq {..} issueHandle identifier becknIssueId = do
  category <- CQIC.findById categoryId identifier >>= fromMaybeM (IssueCategoryDoesNotExist categoryId.getId)
  mbOption <- forM optionId \justOptionId -> do
    issueOption <- CQIO.findById justOptionId identifier >>= fromMaybeM (IssueOptionDoesNotExist justOptionId.getId)
    whenJust issueOption.issueCategoryId $ \optionCategoryId ->
      when (optionCategoryId /= categoryId) $ throwError (IssueOptionInvalid justOptionId.getId categoryId.getId)
    return issueOption
  mbRide <- forM rideId \justRideId -> do
    B.runInReplica (issueHandle.findRideById justRideId merchantId) >>= fromMaybeM (RideNotFound justRideId.getId)
  uploadedMediaFiles <- forM mediaFiles $ \mediaFile ->
    CQMF.findById mediaFile identifier >>= fromMaybeM (FileDoesNotExist mediaFile.getId)
  person <- issueHandle.findPersonById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let mediaFileUrls = map (.url) uploadedMediaFiles
      mocId = maybe person.merchantOperatingCityId (.merchantOperatingCityId) mbRide
  language <- getLanguage personId mbLanguage issueHandle
  issueConfig <- CQI.findByMerchantOpCityId mocId identifier >>= fromMaybeM (IssueConfigNotFound mocId.getId)
  let shouldCreateTicket = isNothing createTicket || fromJust createTicket
      onCreateIssueMsgs = if shouldCreateTicket then issueConfig.onCreateIssueMsgs else []
  issueMessageTranslationList <- mapM (\messageId -> CQIM.findByIdAndLanguage messageId language identifier) onCreateIssueMsgs
  now <- getCurrentTime
  mbRideInfoRes <- traverse (issueHandle.getRideInfo merchantId mocId) rideId
  let messages = mkIssueMessageList (Just $ catMaybes issueMessageTranslationList) language issueConfig mbRideInfoRes
      chats_ = fromMaybe [] chats
      updatedChats = updateChats chats_ shouldCreateTicket messages uploadedMediaFiles now
  config <- issueHandle.findMerchantConfig merchantId mocId (Just personId)
  processIssueReportTypeActions (personId, merchantId) mbOption mbRide (Just config) True identifier issueHandle
  issueReport <- mkIssueReport mocId updatedChats shouldCreateTicket now
  let isLOFeedback = (identifier == CUSTOMER) && checkForLOFeedback config.sensitiveWords config.sensitiveWordsForExactMatch (Just description)
  when isLOFeedback $
    fork "notify on slack" $ do
      sosAlertsTopicARN <- asks (.sosAlertsTopicARN)
      moCity <-
        issueHandle.findMOCityById mocId
          >>= fromMaybeM (MerchantOperatingCityNotFound $ "MerchantOpCityId - " <> show mocId)
      msg <- generateMsg moCity mbRide mbRideInfoRes person description
      let message = createJsonMessage msg
      void $ L.runIO $ Slack.publishMessage sosAlertsTopicARN message
  _ <- QIR.create issueReport
  when shouldCreateTicket $ do
    ticket <- buildTicket issueReport category mbOption mbRide mbRideInfoRes person mocId config mediaFileUrls now issueHandle
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
            chats = updatedChats,
            merchantId = Just merchantId,
            reopenedCount = 0,
            becknIssueId
          }
    createJsonMessage :: Text -> T.Text
    createJsonMessage descriptionText =
      let jsonValue :: A.Value
          jsonValue =
            A.object
              [ "version" A..= ("1.0" :: String),
                "source" A..= ("custom" :: String),
                "content"
                  A..= A.object
                    [ "description" A..= descriptionText
                    ]
              ]
       in TL.toStrict $ TLE.decodeUtf8 $ A.encode jsonValue

    generateMsg :: (BeamFlow m r, EncFlow m r) => MerchantOperatingCity -> Maybe Ride -> Maybe RideInfoRes -> Person -> Text -> m Text
    generateMsg moCity mbRide mbRideInfoRes person desc = do
      mbCustomerPhone <- mapM decrypt person.mobileNumber
      now <- getCurrentTime
      let customerName = (fromMaybe "" person.firstName) <> " " <> (fromMaybe "" person.lastName)
          customerPhone = fromMaybe "N/A" mbCustomerPhone
          currentTimeInIST = showTimeIst now
      case (mbRide, mbRideInfoRes) of
        (Just ride, Just rideInfo) -> do
          let driverPhoneNumber = fromMaybe "N/A" rideInfo.driverPhoneNo
              vehicleVariant = maybe "N/A" show (rideInfo.vehicleVariant)
              dropLocation = fromMaybe "N/A" (rideInfo.customerDropLocation >>= \loc -> Just ("area - " <> fromMaybe "N/A" (loc.area) <> ", street - " <> fromMaybe "N/A" (loc.street)))
              fromLocation = rideInfo.customerPickupLocation
              pickupLocation = "area - " <> fromMaybe "N/A" (fromLocation.area) <> ", street - " <> fromMaybe "N/A" (fromLocation.street)
              rideStartTime = maybe "N/A" showTimeIst rideInfo.rideStartTime
          return $
            "There is an L0 feedback/report given by Customer Name : " <> customerName <> "\n"
              <> "Customer Phone No : "
              <> customerPhone
              <> "\n"
              <> "Driver Name : "
              <> rideInfo.driverName
              <> "\n"
              <> "Driver Phone No : "
              <> driverPhoneNumber
              <> "\n"
              <> "City : "
              <> show moCity.city
              <> "\n"
              <> "Variant : "
              <> vehicleVariant
              <> "\n"
              <> "Pickup Location : "
              <> pickupLocation
              <> "\n"
              <> "Drop Location : "
              <> dropLocation
              <> "\n"
              <> "Ride Id : "
              <> ride.id.getId
              <> "\n"
              <> "Ride Start Time : "
              <> rideStartTime
              <> "\n"
              <> "Feedback Time : "
              <> currentTimeInIST
              <> "\n"
              <> "Feedback Message : "
              <> desc
        _ -> do
          return $
            "There is an L0 feedback/report given by Customer Name : " <> customerName <> "\n"
              <> "Customer Phone No : "
              <> customerPhone
              <> "\n"
              <> "Feedback Time : "
              <> currentTimeInIST
              <> "\n"
              <> "Feedback Message : "
              <> desc
              <> "\n"
              <> "City : "
              <> show moCity.city
              <> "\n"

    buildTicket :: (EncFlow m r, BeamFlow m r) => D.IssueReport -> D.IssueCategory -> Maybe D.IssueOption -> Maybe Ride -> Maybe RideInfoRes -> Person -> Id MerchantOperatingCity -> MerchantConfig -> [Text] -> UTCTime -> ServiceHandle m -> m TIT.CreateTicketReq
    buildTicket issue category mbOption mbRide mbRideInfoRes person moCityId merchantCfg mediaFileUrls now iHandle = do
      info <- buildRideInfo moCityId now mbRide mbRideInfoRes iHandle
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
            classification = castIdentifierToClassification identifier,
            rideDescription = Just info,
            becknIssueId
          }

    buildRideInfo :: (BeamFlow m r, EncFlow m r) => Id MerchantOperatingCity -> UTCTime -> Maybe Ride -> Maybe RideInfoRes -> ServiceHandle m -> m TIT.RideInfo
    buildRideInfo moCityId now mbRide mbRideInfoRes iHandle = do
      moCity <-
        iHandle.findMOCityById moCityId
          >>= fromMaybeM (MerchantOperatingCityNotFound $ "MerchantOpCityId - " <> show moCityId)
      return
        TIT.RideInfo
          { rideShortId = maybe "" (.shortId.getShortId) mbRide,
            rideCity = show moCity.city,
            customerName = (.customerName) =<< mbRideInfoRes,
            customerPhoneNo = (.customerPhoneNo) <$> mbRideInfoRes,
            driverName = (.driverName) <$> mbRideInfoRes,
            driverPhoneNo = (.driverPhoneNo) =<< mbRideInfoRes,
            vehicleNo = maybe "" (.vehicleNo) mbRideInfoRes,
            vehicleCategory = show . fromJust . vehicleVariant <$> mbRideInfoRes,
            vehicleServiceTier = (.vehicleServiceTierName) =<< mbRideInfoRes,
            status = maybe "" (show . (.bookingStatus)) mbRideInfoRes,
            rideCreatedAt = maybe now (.createdAt) mbRide,
            pickupLocation = mkLocation ((.customerPickupLocation) <$> mbRideInfoRes),
            dropLocation = mkLocation . (.customerDropLocation) <$> mbRideInfoRes,
            fare = (.actualFare) =<< mbRideInfoRes
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
  (Id Person, Id Merchant, Id MerchantOperatingCity) ->
  Maybe Language ->
  ServiceHandle m ->
  Identifier ->
  m Common.IssueInfoRes
issueInfo issueReportId (personId, merchantId, merchantOpCityId) mbLanguage issueHandle identifier = do
  language <- getLanguage personId mbLanguage issueHandle
  issueReport <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoesNotExist issueReportId.getId)
  mediaFiles <- CQMF.findAllInForIssueReportId issueReport.mediaFiles issueReportId identifier
  mbRideInfoRes <- mapM (issueHandle.getRideInfo merchantId merchantOpCityId) issueReport.rideId
  let adjMerchantOpCityId = maybe merchantOpCityId Id ((.merchantOperatingCityId) =<< mbRideInfoRes)
  issueConfig <-
    CQI.findByMerchantOpCityId adjMerchantOpCityId identifier
      >>= fromMaybeM (IssueConfigNotFound adjMerchantOpCityId.getId)
  (issueCategory, _) <- CQIC.findByIdAndLanguage issueReport.categoryId language identifier >>= fromMaybeM (IssueCategoryNotFound issueReport.categoryId.getId)
  mbIssueOption <- (join <$>) $
    forM issueReport.optionId $ \justIssueOption -> do
      CQIO.findByIdAndLanguage justIssueOption language identifier
  issueChats <- recreateIssueChats issueReport issueConfig mbRideInfoRes language identifier
  issueOptions <-
    if null issueReport.chats
      then pure []
      else CQIO.findAllActiveByMessageAndLanguage (Id (last issueReport.chats).chatId) language identifier
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
        options = map (mkIssueOptionList issueConfig language mbRideInfoRes) issueOptions,
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
  void $ QIR.findById issueReportId >>= fromMaybeM (IssueReportDoesNotExist issueReportId.getId)
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
  issueReport <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoesNotExist issueReportId.getId)
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
  issueReport <- QIR.findById issueReportId >>= fromMaybeM (IssueReportDoesNotExist issueReportId.getId)
  mbRideInfoRes <- mapM (issueHandle.getRideInfo merchantId merchantOpCityId) issueReport.rideId
  case status of
    CLOSED -> do
      QIR.updateStatusAssignee issueReport.id (Just status) issueReport.assignee
      pure $
        Common.IssueStatusUpdateRes
          { messages = []
          }
    REOPENED -> do
      QIR.updateIssueReopenedCount issueReportId (issueReport.reopenedCount + 1)
      QIR.updateStatusAssignee issueReport.id (Just status) issueReport.assignee
      updateTicketStatus issueReport TIT.CRS merchantId merchantOpCityId issueHandle "Ticket reopened"
      issueConfig <- CQI.findByMerchantOpCityId merchantOpCityId identifier >>= fromMaybeM (InternalError "IssueConfigNotFound")
      issueMessageTranslation <- mapM (\messageId -> CQIM.findByIdAndLanguage messageId language identifier) issueConfig.onIssueReopenMsgs
      let issueMessages = mkIssueMessageList (sequence issueMessageTranslation) language issueConfig mbRideInfoRes
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

processIssueReportTypeActions ::
  BeamFlow m r =>
  (Id Person, Id Merchant) ->
  Maybe D.IssueOption ->
  Maybe Ride ->
  Maybe MerchantConfig ->
  Bool ->
  Identifier ->
  ServiceHandle m ->
  m ()
processIssueReportTypeActions (personId, merchantId) mbIssueOption mbRide mbMerchantConfig isIssueReportCreated identifier issueHandle = do
  let mbIssueReportType = mbIssueOption >>= (.label) >>= A.decode . A.encode
  case mbIssueReportType of
    Just AC_RELATED_ISSUE -> processExternalIssueReporting AC_RELATED_ISSUE issueHandle
    Just DRIVER_TOLL_RELATED_ISSUE -> processExternalIssueReporting DRIVER_TOLL_RELATED_ISSUE issueHandle
    Just SYNC_BOOKING -> processBookingSyncReq issueHandle
    Just EXTRA_FARE_MITIGATION -> processExternalIssueReporting EXTRA_FARE_MITIGATION issueHandle
    Nothing -> return ()
  where
    processBookingSyncReq :: BeamFlow m r => ServiceHandle m -> m ()
    processBookingSyncReq iHandle =
      case (iHandle.mbSyncRide, iHandle.mbFindLatestBookingByPersonId, iHandle.mbFindRideByBookingId, isIssueReportCreated) of
        (Just syncRide, Just findLatestBookingByPersonId, Just findRideByBookingId, False) -> do
          mbLatestBooking <- findLatestBookingByPersonId personId
          whenJust mbLatestBooking $ \latestBooking -> do
            mbLatestRide <- findRideByBookingId latestBooking.id merchantId
            whenJust mbLatestRide $ \latestRide -> do
              syncRes <- try @_ @SomeException $ syncRide latestRide.merchantId latestRide.id
              case syncRes of
                Right _ -> pure ()
                Left err -> logTagInfo "Failed to sync ride for rideId-" (latestRide.id.getId <> "-error-" <> show err)
        _ -> pure ()

    processExternalIssueReporting :: BeamFlow m r => IssueReportType -> ServiceHandle m -> m ()
    processExternalIssueReporting issueReportType iHandle =
      whenJust iHandle.mbReportIssue $ \reportIssue -> do
        let mbCounterPartyUrl = (.counterPartyUrl) <$> mbMerchantConfig
            mbCounterPartyApiKey = (.counterPartyApiKey) <$> mbMerchantConfig
        case (mbRide, mbCounterPartyUrl, mbCounterPartyApiKey, isIssueReportCreated) of
          (Just ride, Just counterPartyUrl, Just counterPartyApiKey, True) -> do
            whenJust ride.counterPartyRideId $ \counterPRideId -> do
              checkForExistingIssues issueReportType ride.id
              reportIssueAPIRes <- try @_ @SomeException $ reportIssue counterPartyUrl counterPartyApiKey counterPRideId issueReportType
              case reportIssueAPIRes of
                Right _ -> pure ()
                Left err -> case issueReportType of
                  AC_RELATED_ISSUE -> handleACIssueActions counterPartyUrl counterPartyApiKey iHandle
                  _ -> logTagInfo "Report Issue API failed for " $ show err
          _ -> pure ()

    checkForExistingIssues :: BeamFlow m r => IssueReportType -> Id Ride -> m ()
    checkForExistingIssues issueReportType rideId = do
      issueList <- QIR.findAllByPersonAndRideId personId rideId
      mapM_
        ( \issueRep -> do
            mbIssueRepOption <- maybe (return Nothing) (`CQIO.findById` identifier) issueRep.optionId
            whenJust mbIssueRepOption $ \iOption -> do
              let parsedLabel = A.decode . A.encode =<< iOption.label
              when (parsedLabel == Just issueReportType) $ throwError (IssueReportAlreadyExists rideId.getId)
        )
        issueList

    handleACIssueActions :: BeamFlow m r => BaseUrl -> Text -> ServiceHandle m -> m ()
    handleACIssueActions counterPartyUrl counterPartyApiKey iHandle =
      whenJust iHandle.mbReportACIssue $ \reportACIssue ->
        whenJust (mbRide >>= (.counterPartyRideId)) $ \counterPRideId -> do
          acIssueApiRes <- try @_ @SomeException $ reportACIssue counterPartyUrl counterPartyApiKey counterPRideId
          case acIssueApiRes of
            Right _ -> pure ()
            Left err -> logTagInfo "Report AC Issue API failed - " $ show err

mkIssueMessageList ::
  Maybe [(D.IssueMessage, D.DetailedTranslation, [Text])] ->
  Language ->
  D.IssueConfig ->
  Maybe RideInfoRes ->
  [Common.Message]
mkIssueMessageList mbList language issueConfig mbRideInfoRes = case mbList of
  Nothing -> []
  Just list ->
    map
      ( \(issueMessage, detailedTranslation, mediaFileUrls) -> do
          let message = fromMaybe issueMessage.message (detailedTranslation.contentTranslation <&> (.translation))
              messageTitle = (detailedTranslation.titleTranslation <&> (.translation)) <|> issueMessage.messageTitle
              messageAction = (detailedTranslation.actionTranslation <&> (.translation)) <|> issueMessage.messageAction
          Common.Message
            { id = issueMessage.id,
              message = messageTransformer mediaFileUrls message,
              messageTitle = messageTransformer mediaFileUrls <$> messageTitle,
              messageAction = messageTransformer mediaFileUrls <$> messageAction,
              label = fromMaybe "" issueMessage.label,
              mediaFileUrls,
              referenceCategoryId = issueMessage.referenceCategoryId,
              referenceOptionId = issueMessage.referenceOptionId
            }
      )
      list
  where
    messageTransformer :: [Text] -> Text -> Text
    messageTransformer = transformText (getConfigValue language issueConfig mbRideInfoRes)

transformText :: (Text -> Text) -> [Text] -> Text -> Text
transformText getCfgValue mediaFileUrls text =
  T.strip $ replaceNewLineChar $ fst $ foldl' replacePatterns (text, mediaFileUrls) patterns
  where
    patterns :: [Text]
    patterns = do
      let regex :: Text = "\\{#[^#]*#\\}"
      getAllTextMatches (text =~ regex :: AllTextMatches [] Text)

    replacePatterns :: (Text, [Text]) -> Text -> (Text, [Text])
    replacePatterns (msg, urls) pattern_
      | "{#IMAGE_GROUP" `T.isPrefixOf` pattern_ =
        replaceImageGroup pattern_ urls msg
      | pattern_ == "{#IMAGE#}" =
        (replaceFirstImage pattern_ urls msg, drop 1 urls)
      | otherwise =
        let key = extractKey pattern_
            value = fromMaybe pattern_ (Just (getCfgValue key))
         in (T.replace pattern_ value msg, urls)

    extractKey :: Text -> Text
    extractKey pattern_ = T.drop 2 $ T.dropEnd 2 pattern_

    replaceNewLineChar :: Text -> Text
    replaceNewLineChar = T.replace "\n" "<br>"

    replaceFirstImage :: Text -> [Text] -> Text -> Text
    replaceFirstImage pattern_ urls txt =
      let (before, after) = T.breakOn pattern_ txt
       in if T.null after || null urls
            then txt
            else before <> (" {SUBPART}{IMAGE}{!!!} " <> head urls <> " ") <> T.drop (T.length pattern_) after

    replaceImageGroup :: T.Text -> [T.Text] -> T.Text -> (T.Text, [T.Text])
    replaceImageGroup pattern_ urls txt
      | null urls = (txt, urls)
      | otherwise =
        let (before, after) = T.breakOn pattern_ txt
            countText = T.takeWhile (/= '>') $ T.drop 20 pattern_
            count = maybe 0 (\n -> n) (readMaybe (T.unpack countText) :: Maybe Int)
            (groupUrls, remainingUrls) = splitAt count urls
            replacement = T.unwords $ map (\url -> "{IMAGE} " <> url <> " ") groupUrls
            newText = before <> "{SUBPART}{IMAGE_GROUP}{!!!} " <> replacement <> T.drop (T.length pattern_) after
         in (newText, remainingUrls)

getConfigValue :: Language -> D.IssueConfig -> Maybe RideInfoRes -> Text -> Text
getConfigValue language issueConfig mbRideInfoRes key = do
  let estimatedDistance = fromMaybe 0.0 $ (.estimatedDistance) =<< mbRideInfoRes
      chargeableDistance = fromMaybe 0.0 $ (.chargeableDistance) =<< mbRideInfoRes
      distanceDifference = estimatedDistance - chargeableDistance
      estimatedFare = maybe (HighPrecMoney 0.0) (.estimatedFare) mbRideInfoRes
      finalFare = fromMaybe 0.0 $ (.computedPrice) =<< mbRideInfoRes
      fareDifference = estimatedFare - finalFare
      fareBreakup = maybe [] (.fareBreakup) mbRideInfoRes
      driverPickupCharges = maybe 0.0 (.amount.amount) (getFareFromArray "DEAD_KILOMETER_FARE" fareBreakup)
      tollCharges = maybe 0.0 (.amount.amount) (getFareFromArray "TOLL_CHARGES" fareBreakup)
      tipAdded = maybe 0.0 (.amount.amount) (getFareFromArray "CUSTOMER_SELECTED_FARE" fareBreakup)
      driverAdditions = maybe 0.0 (.amount.amount) (getFareFromArray "DRIVER_SELECTED_FARE" fareBreakup)
      vehicleNo = maybe "" (formatVehicleNo . (.vehicleNo)) mbRideInfoRes
      driverName = maybe "" (.driverName) mbRideInfoRes
      driverPhoneNo = fromMaybe "" $ (.driverPhoneNo) =<< mbRideInfoRes
      vehicleServiceTierName = fromMaybe "" $ (.vehicleServiceTierName) =<< mbRideInfoRes
      mobileCountryCode = fromMaybe "+91" $ (.mobileCountryCode) =<< mbRideInfoRes
      merchantName =
        fromMaybe "" $
          issueConfig.messageTransformationConfig
            >>= ( \config ->
                    ((.translation) <$> (find (\t -> t.language == language) =<< config.merchantNameWTranslations))
                      <|> config.merchantName
                )
   in case key of
        "MERCHANT" -> merchantName
        "SUPPORT_MAIL" -> fromMaybe "" ((.supportEmail) =<< issueConfig.messageTransformationConfig)
        "ESTIMATED_DISTANCE" -> show estimatedDistance
        "FINAL_DISTANCE" -> show chargeableDistance
        "DISTANCE_DIFFERENCE" -> show distanceDifference
        "ESTIMATED_FARE" -> show estimatedFare
        "FINAL_FARE" -> show finalFare
        "FARE_DIFFERENCE" -> show fareDifference
        "FARE_CORRELATION" -> bool "remained same" (bool ("was decreased by ₹" <> show fareDifference) ("was increased by ₹" <> show fareDifference) (finalFare > estimatedFare)) (finalFare == estimatedFare)
        "DISTANCE_CORRELATION" -> bool "no change" (bool ("a " <> show distanceDifference <> " km decrease") ("a " <> show distanceDifference <> " km increase") (chargeableDistance > estimatedDistance)) (chargeableDistance == estimatedDistance)
        "FARE_ARROW" -> bool (bool " ↓" " ↑" (finalFare > estimatedFare)) "" (finalFare == estimatedFare)
        "DISTANCE_ARROW" -> bool (bool " ↓" " ↑" (chargeableDistance > estimatedDistance)) "" (chargeableDistance == estimatedDistance)
        "DRIVER_PICKUP_CHARGE" -> show driverPickupCharges
        "TOLL_CHARGES" -> show tollCharges
        "TIP_ADDED" -> show tipAdded
        "DRIVER_ADDITIONS" -> show driverAdditions
        "HEADING" -> "{SUBPART}{HEADING}{!!!}"
        "BODY" -> "{SUBPART}{BODY}{!!!}"
        "VEHICLE_NUMBER" -> vehicleNo
        "DRIVER_NAME" -> driverName
        "DRIVER_MOBILE_NUMBER" -> mobileCountryCode <> " " <> driverPhoneNo
        "VEHICLE_SERVICE_TIER" -> vehicleServiceTierName
        others -> "{#" <> others <> "#}"
  where
    getFareFromArray :: Text -> [FareBreakup] -> Maybe FareBreakup
    getFareFromArray fareKey = find (\fareBreakup -> fareBreakup.description == fareKey)

    formatVehicleNo :: Text -> Text
    formatVehicleNo vehicleNo =
      let pattern_ = "^([A-Z]{2})([0-9]{2})([A-Z]*)([0-9]{4})$" :: String
          vehicleStr = T.unpack vehicleNo
          matches = vehicleStr =~ pattern_ :: (String, String, String, [String])
       in case matches of
            (_, _, _, [grp1, grp2, grp3, grp4]) ->
              let formatted = unwords [grp1, grp2] ++ (if null grp3 then "" else " " ++ grp3) ++ " " ++ grp4
               in T.pack formatted
            _ -> vehicleNo

mkIssueOptionList ::
  D.IssueConfig -> Language -> Maybe RideInfoRes -> (D.IssueOption, Maybe D.IssueTranslation) -> Common.IssueOptionRes
mkIssueOptionList issueConfig language mbRideInfoRes (issueOption, issueTranslation) =
  Common.IssueOptionRes
    { issueOptionId = issueOption.id,
      label = fromMaybe (issueOption.option & T.toUpper & T.replace " " "_") issueOption.label,
      option = transformOption $ fromMaybe issueOption.option $ issueTranslation <&> (.translation),
      mandatoryUploads = issueOption.mandatoryUploads
    }
  where
    transformOption :: Text -> Text
    transformOption = transformText (getConfigValue language issueConfig mbRideInfoRes) []

recreateIssueChats :: BeamFlow m r => D.IssueReport -> D.IssueConfig -> Maybe RideInfoRes -> Language -> Identifier -> m [ChatDetail]
recreateIssueChats issueReport issueConfig mbRideInfoRes language identifier =
  mapM
    ( \item -> case item.chatType of
        IssueMessage -> do
          mbIssueMessageTranslation <- CQIM.findByIdAndLanguage (Id item.chatId) language identifier
          let mbMessage = (\messageList -> listToMaybe $ mkIssueMessageList (Just messageList) language issueConfig mbRideInfoRes) . (: []) =<< mbIssueMessageTranslation
          let label = (.label) . fst_ =<< mbIssueMessageTranslation
          pure $ mkChatDetail item.chatId item.timestamp Text BOT (mbMessage <&> (.message)) (mbMessage >>= (.messageTitle)) (mbMessage >>= (.messageAction)) label
        IssueOption -> do
          mbIssueOptionTranslation <- CQIO.findByIdAndLanguage (Id item.chatId) language identifier
          let mbIssueOption = mkIssueOptionList issueConfig language mbRideInfoRes <$> mbIssueOptionTranslation
              label = (.label) . fst =<< mbIssueOptionTranslation
          pure $ mkChatDetail item.chatId item.timestamp Text USER (mbIssueOption <&> (.option)) Nothing Nothing label
        IssueDescription -> pure $ mkChatDetail item.chatId item.timestamp Text USER (Just issueReport.description) Nothing Nothing Nothing
        MediaFile -> do
          mediaFile <- CQMF.findById (Id item.chatId) identifier >>= fromMaybeM (FileDoesNotExist item.chatId)
          pure $ mkChatDetail item.chatId item.timestamp (mediaTypeToMessageType mediaFile._type) USER (Just mediaFile.url) Nothing Nothing Nothing
    )
    issueReport.chats
  where
    mediaTypeToMessageType :: S3.FileType -> MessageType
    mediaTypeToMessageType = \case
      S3.Audio -> Audio
      S3.Image -> Image
      _ -> Text

    fst_ (x, _, _) = x

    mkChatDetail id timestamp chatType sender content title actionText label =
      Common.ChatDetail {..}

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
