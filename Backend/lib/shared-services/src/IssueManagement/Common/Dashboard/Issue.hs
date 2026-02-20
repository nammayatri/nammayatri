{-# LANGUAGE DerivingVia #-}

module IssueManagement.Common.Dashboard.Issue
  ( module IssueManagement.Common.Dashboard.Issue,
    module Reexport,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE
import qualified IGM.Enums as Spec
import IssueManagement.Common as Common
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueConfig (IssueConfig)
import IssueManagement.Domain.Types.Issue.IssueMessage (IssueMessage)
import qualified IssueManagement.Domain.Types.Issue.IssueMessage as IssueMessage
import IssueManagement.Domain.Types.Issue.IssueOption
import IssueManagement.Domain.Types.Issue.IssueReport
import IssueManagement.Domain.Types.MediaFile (MediaFile)
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Types.CacheFlow as Reexport
import Kernel.Types.Common
import Kernel.Types.HideSecrets as Reexport
import Kernel.Types.Id

---------------------------------------------------------
-- issue category list --------------------------------

data IssueCategoryRes = IssueCategoryRes
  { issueCategoryId :: Id IssueCategory,
    label :: Text,
    category :: Text,
    logoUrl :: Text,
    categoryType :: CategoryType,
    isRideRequired :: Bool,
    isTicketRequired :: Bool,
    maxAllowedRideAge :: Maybe Seconds,
    allowedRideStatuses :: Maybe [RideStatus]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype IssueCategoryListRes = IssueCategoryListRes
  { categories :: [IssueCategoryRes]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- issues list --------------------------------------

data IssueReportListResponse = IssueReportListResponse
  { issues :: [IssueReportListItem],
    summary :: Summary
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueReportListItem = IssueReportListItem
  { issueReportId :: Id IssueReport,
    issueReportShortId :: Maybe (ShortId IssueReport),
    personId :: Id Common.Person,
    rideId :: Maybe (Id Ride),
    ticketBookingId :: Maybe (Id FRFSTicketBooking),
    deleted :: Bool,
    category :: Text,
    categoryId :: Maybe (Id IssueCategory),
    assignee :: Maybe Text,
    status :: IssueStatus,
    createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- issue info --------------------------------

data IssueInfoDRes = IssueInfoDRes
  { issueReportId :: Id IssueReport,
    issueReportShortId :: Maybe (ShortId IssueReport),
    personDetail :: Maybe PersonDetail,
    rideId :: Maybe (Id Ride),
    ticketBookingId :: Maybe (Id FRFSTicketBooking),
    chats :: Maybe [ChatDetail],
    comments :: [IssueReportCommentItem],
    category :: Maybe Text,
    mediaFiles :: [MediaFile],
    option :: Maybe Text,
    description :: Text,
    status :: IssueStatus,
    assignee :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PersonDetail = PersonDetail
  { personId :: Id Common.Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueReportCommentItem = IssueReportCommentItem
  { comment :: Text,
    authorDetail :: AuthorDetail,
    timestamp :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AuthorDetail = AuthorDetail
  { authorId :: Id User,
    firstName :: Maybe Text,
    lastName :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

------------------------------------------------------
-- update issue --------------------------------------

data IssueUpdateReq = IssueUpdateReq
  { status :: Maybe IssueStatus,
    assignee :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets IssueUpdateReq where
  hideSecrets = identity

data IssueUpdateByUserReq = IssueUpdateByUserReq
  { status :: Maybe IssueStatus,
    assignee :: Maybe Text,
    userId :: Id User
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets IssueUpdateByUserReq where
  hideSecrets = identity

---------------------------------------------------------
-- add comment --------------------------------------

newtype IssueAddCommentReq = IssueAddCommentReq
  { comment :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets IssueAddCommentReq where
  hideSecrets = identity

data IssueAddCommentByUserReq = IssueAddCommentByUserReq
  { comment :: Text,
    userId :: Id User
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets IssueAddCommentByUserReq where
  hideSecrets = identity

---------------------------------------------------------
-- Ticket Status Call Back --------------------------------------

data TicketStatusCallBackReq = TicketStatusCallBackReq
  { ticketId :: Text,
    status :: Text,
    subStatus :: Maybe Text,
    queue :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToSchema)

instance FromJSON TicketStatusCallBackReq where
  parseJSON = withObject "TicketStatusCallBackReq" $ \v ->
    TicketStatusCallBackReq
      <$> v .: "ticketId"
      <*> v .: "status"
      <*> parseSubStatus v
      <*> v .:? "queue"
    where
      parseSubStatus :: Object -> Parser (Maybe Text)
      parseSubStatus v = firstJustM (v .:?) ["subStatus", "sub_status", "sub-status"]

      firstJustM :: Monad m => (k -> m (Maybe v)) -> [k] -> m (Maybe v)
      firstJustM _ [] = return Nothing
      firstJustM f (x : xs) = do
        result <- f x
        case result of
          Just val -> return $ Just val
          Nothing -> firstJustM f xs

instance ToJSON TicketStatusCallBackReq

instance HideSecrets TicketStatusCallBackReq where
  hideSecrets = identity

data Summary = Summary
  { totalCount :: Int,
    count :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-----------------------------------------------------------
-- CreateIssueCatgeory API ------------------------------------

data CreateIssueCategoryReq = CreateIssueCategoryReq
  { category :: Text,
    logoUrl :: Text,
    priority :: Int,
    categoryType :: CategoryType,
    isRideRequired :: Bool,
    isTicketRequired :: Bool,
    maxAllowedRideAge :: Maybe Seconds,
    allowedRideStatuses :: Maybe [RideStatus],
    isActive :: Maybe Bool,
    translations :: [Translation],
    messages :: [CreateIssueMessageReq],
    label :: Maybe Text,
    igmCategory :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateIssueMessageReq = CreateIssueMessageReq
  { message :: Text,
    messageTitle :: Maybe Text,
    messageAction :: Maybe Text,
    label :: Maybe Text,
    priority :: Int,
    messageTranslations :: [Translation],
    titleTranslations :: [Translation],
    actionTranslations :: [Translation],
    options :: [CreateIssueOptionReq],
    referenceOptionId :: Maybe (Id IssueOption),
    referenceCategoryId :: Maybe (Id IssueCategory),
    isActive :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateIssueOptionReq = CreateIssueOptionReq
  { option :: Text,
    label :: Maybe Text,
    priority :: Int,
    isActive :: Maybe Bool,
    translations :: [Translation],
    messages :: [CreateIssueMessageReq],
    restrictedVariants :: Maybe [VehicleVariant],
    restrictedRideStatuses :: Maybe [RideStatus],
    showOnlyWhenUserBlocked :: Maybe Bool,
    igmSubCategory :: Maybe Spec.IssueSubCategory,
    mandatoryUploads :: Maybe [Common.MandatoryUploads]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype CreateIssueCategoryRes = CreateIssueCategoryRes
  { categoryId :: Id IssueCategory
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets CreateIssueCategoryReq where
  hideSecrets = identity

-----------------------------------------------------------
-- Update IssueCatgeory API ------------------------------------

data UpdateIssueCategoryReq = UpdateIssueCategoryReq
  { category :: Maybe Text,
    logoUrl :: Maybe Text,
    priority :: Maybe Int,
    isRideRequired :: Maybe Bool,
    isTicketRequired :: Maybe Bool,
    maxAllowedRideAge :: Maybe Seconds,
    allowedRideStatuses :: Maybe [RideStatus],
    isActive :: Maybe Bool,
    translations :: [Translation],
    label :: Maybe Text,
    igmCategory :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UpdateIssueCategoryReq where
  hideSecrets = identity

-----------------------------------------------------------
-- Create IssueOption API ------------------------------------

newtype CreateIssueOptionRes = CreateIssueOptionRes
  { optionId :: Id IssueOption
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets CreateIssueOptionReq where
  hideSecrets = identity

--------------------------------------------------------------
-- Update IssueOption API ------------------------------------

data UpdateIssueOptionReq = UpdateIssueOptionReq
  { issueCategoryId :: Maybe (Id IssueCategory),
    option :: Maybe Text,
    priority :: Maybe Int,
    issueMessageId :: Maybe (Id IssueMessage),
    label :: Maybe Text,
    isActive :: Maybe Bool,
    translations :: [Translation],
    restrictedVariants :: Maybe [VehicleVariant],
    restrictedRideStatuses :: Maybe [RideStatus],
    showOnlyWhenUserBlocked :: Maybe Bool,
    igmCategory :: Maybe Text,
    mandatoryUploads :: Maybe [Common.MandatoryUploads]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UpdateIssueOptionReq where
  hideSecrets = identity

-----------------------------------------------------------
-- Upsert IssueMessage API ------------------------------------

data UpsertIssueMessageReq = UpsertIssueMessageReq
  { issueMessageId :: Maybe (Id IssueMessage),
    message :: Maybe Text,
    messageTitle :: Maybe Text,
    messageAction :: Maybe Text,
    categoryId :: Maybe (Id IssueCategory),
    optionId :: Maybe (Id IssueOption),
    priority :: Maybe Int,
    label :: Maybe Text,
    messageTranslations :: Maybe [Translation],
    titleTranslations :: Maybe [Translation],
    actionTranslations :: Maybe [Translation],
    referenceOptionId :: Maybe (Id IssueOption),
    referenceCategoryId :: Maybe (Id IssueCategory),
    isActive :: Maybe Bool,
    deleteExistingFiles :: Maybe Bool,
    mediaFiles :: Maybe [IssueMessageMediaFileUploadReq]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype UpsertIssueMessageRes = UpsertIssueMessageRes
  { messageId :: Id IssueMessage
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueMessageMediaFileUploadReq = IssueMessageMediaFileUploadReq
  { file :: FilePath,
    reqContentType :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp UpsertIssueMessageReq where
  fromMultipart form = do
    mediaFiles <- mapM extractFile (files form)
    UpsertIssueMessageReq
      <$> parseMaybeField "issueMessageId"
      <*> parseMaybeField "message"
      <*> parseMaybeField "messageTitle"
      <*> parseMaybeField "messageAction"
      <*> parseMaybeField "categoryId"
      <*> parseMaybeField "optionId"
      <*> parseMaybeInput "priority"
      <*> parseMaybeField "label"
      <*> parseMaybeJsonInput "messageTranslations"
      <*> parseMaybeJsonInput "titleTranslations"
      <*> parseMaybeJsonInput "actionTranslations"
      <*> parseMaybeField "referenceOptionId"
      <*> parseMaybeField "referenceCategoryId"
      <*> parseMaybeInput "isActive"
      <*> parseMaybeInput "deleteExistingFiles"
      <*> pure (Just mediaFiles)
    where
      extractFile f = pure $ IssueMessageMediaFileUploadReq (fdPayload f) (fdFileCType f)

      parseMaybeInput :: Read b => Text -> Either String (Maybe b)
      parseMaybeInput fieldName = case lookupInput fieldName form of
        Right val -> Right $ readMaybe (T.unpack val)
        Left _ -> Right Nothing

      parseMaybeField :: FromJSON b => Text -> Either String (Maybe b)
      parseMaybeField fieldName = case lookupInput fieldName form of
        Right val -> Right . decode $ encode val
        Left _ -> Right Nothing

      parseMaybeJsonInput :: FromJSON a => Text -> Either String (Maybe a)
      parseMaybeJsonInput fieldName = case lookupInput fieldName form of
        Right val -> case eitherDecodeStrict (DTE.encodeUtf8 val) of
          Right parsedVal -> Right (Just parsedVal)
          Left err -> Left $ "JSON decoding error for field " ++ T.unpack fieldName ++ ": " ++ err
        Left _ -> Right Nothing

instance ToMultipart Tmp UpsertIssueMessageReq where
  toMultipart req = MultipartData inputs files
    where
      inputs =
        catMaybes
          [ fmap (Input "issueMessageId") (getId <$> req.issueMessageId),
            fmap (Input "message") req.message,
            fmap (Input "messageTitle") req.messageTitle,
            fmap (Input "messageAction") req.messageAction,
            fmap (Input "categoryId") (getId <$> req.categoryId),
            fmap (Input "optionId") (getId <$> req.optionId),
            fmap (Input "priority" . T.pack . show) req.priority,
            fmap (Input "label") req.label,
            fmap (Input "messageTranslations" . encodeJson) req.messageTranslations,
            fmap (Input "titleTranslations" . encodeJson) req.titleTranslations,
            fmap (Input "actionTranslations" . encodeJson) req.actionTranslations,
            fmap (Input "referenceOptionId") (getId <$> req.referenceOptionId),
            fmap (Input "referenceCategoryId") (getId <$> req.referenceCategoryId),
            fmap (Input "isActive" . T.pack . show) req.isActive,
            fmap (Input "deleteExistingFiles" . T.pack . show) req.deleteExistingFiles
          ]
      files = foldMap (map mkFileData) req.mediaFiles
      mkFileData (IssueMessageMediaFileUploadReq filePath contType) =
        FileData "file" (T.pack filePath) contType filePath

      encodeJson :: ToJSON a => a -> Text
      encodeJson = DTE.decodeUtf8 . BL.toStrict . encode

instance HideSecrets UpsertIssueMessageReq where
  hideSecrets = identity

-----------------------------------------------------
-- Issue Report V2 API ------------------------------

data IssueReportReqV2 = IssueReportReqV2
  { ticketId :: Text,
    rideId :: Maybe (Id Ride),
    ticketBookingId :: Maybe (Id FRFSTicketBooking),
    personId :: Id Common.Person,
    mediaFiles :: [Text],
    chats :: [Text],
    issueReportType :: Maybe IssueReportType,
    createIssue :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets IssueReportReqV2 where
  hideSecrets = identity

-----------------------------------------------------------
-- Get Category Detail API ---------------------------------

data IssueCategoryDetailRes = IssueCategoryDetailRes
  { category :: IssueCategoryRes,
    translations :: [Translation],
    messages :: [IssueMessageDetailRes],
    options :: [IssueOptionDetailRes]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueMessageDetailRes = IssueMessageDetailRes
  { messageId :: Id IssueMessage,
    message :: Text,
    messageTitle :: Maybe Text,
    messageAction :: Maybe Text,
    label :: Maybe Text,
    priority :: Int,
    messageType :: IssueMessage.IssueMessageType,
    isActive :: Bool,
    mediaFiles :: [MediaFile],
    translations :: DetailedTranslationRes,
    childOptions :: [IssueOptionDetailRes]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DetailedTranslationRes = DetailedTranslationRes
  { titleTranslation :: [Translation],
    contentTranslation :: [Translation],
    actionTranslation :: [Translation]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueOptionDetailRes = IssueOptionDetailRes
  { optionId :: Id IssueOption,
    option :: Text,
    label :: Maybe Text,
    priority :: Int,
    isActive :: Bool,
    translations :: [Translation],
    childMessages :: [IssueMessageDetailRes]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-----------------------------------------------------------
-- List Options API ----------------------------------------

data IssueOptionListDRes = IssueOptionListDRes
  { options :: [IssueOptionDetailRes]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-----------------------------------------------------------
-- List Messages API ---------------------------------------

data IssueMessageListRes = IssueMessageListRes
  { messages :: [IssueMessageDetailRes]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-----------------------------------------------------------
-- Preview Category Flow API -------------------------------

data IssueCategoryFlowPreviewRes = IssueCategoryFlowPreviewRes
  { category :: IssueCategoryRes,
    flowNodes :: [MessageFlowNode]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageFlowNode = MessageFlowNode
  { message :: MessagePreview,
    options :: [OptionFlowNode],
    nextMessage :: Maybe MessageFlowNode
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessagePreview = MessagePreview
  { messageId :: Id IssueMessage,
    message :: Text,
    messageTitle :: Maybe Text,
    messageAction :: Maybe Text,
    label :: Maybe Text,
    messageType :: IssueMessage.IssueMessageType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OptionFlowNode = OptionFlowNode
  { optionId :: Id IssueOption,
    option :: Text,
    label :: Maybe Text,
    childNodes :: [MessageFlowNode]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-----------------------------------------------------------
-- Translation APIs ----------------------------------------

data IssueTranslationListRes = IssueTranslationListRes
  { translations :: [IssueTranslationItem]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueTranslationItem = IssueTranslationItem
  { sentence :: Text,
    language :: Language,
    translation :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BulkUpsertTranslationsReq = BulkUpsertTranslationsReq
  { translations :: [IssueTranslationItem]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets BulkUpsertTranslationsReq where
  hideSecrets = identity

-----------------------------------------------------------
-- IssueConfig APIs ----------------------------------------

data IssueConfigRes = IssueConfigRes
  { issueConfigId :: Id IssueConfig,
    autoMarkIssueClosedDuration :: Double,
    onCreateIssueMsgs :: [Id IssueMessage],
    onIssueReopenMsgs :: [Id IssueMessage],
    onAutoMarkIssueClsMsgs :: [Id IssueMessage],
    onKaptMarkIssueResMsgs :: [Id IssueMessage],
    onIssueCloseMsgs :: [Id IssueMessage],
    reopenCount :: Int,
    merchantName :: Maybe Text,
    supportEmail :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateIssueConfigReq = UpdateIssueConfigReq
  { autoMarkIssueClosedDuration :: Maybe Double,
    onCreateIssueMsgs :: Maybe [Id IssueMessage],
    onIssueReopenMsgs :: Maybe [Id IssueMessage],
    onAutoMarkIssueClsMsgs :: Maybe [Id IssueMessage],
    onKaptMarkIssueResMsgs :: Maybe [Id IssueMessage],
    onIssueCloseMsgs :: Maybe [Id IssueMessage],
    reopenCount :: Maybe Int,
    merchantName :: Maybe Text,
    supportEmail :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UpdateIssueConfigReq where
  hideSecrets = identity

-----------------------------------------------------------
-- Reorder Priority APIs -----------------------------------

newtype ReorderIssueCategoryReq = ReorderIssueCategoryReq
  { categoryOrder :: [(Id IssueCategory, Int)]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets ReorderIssueCategoryReq where
  hideSecrets = identity

newtype ReorderIssueOptionReq = ReorderIssueOptionReq
  { optionOrder :: [(Id IssueOption, Int)]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets ReorderIssueOptionReq where
  hideSecrets = identity

newtype ReorderIssueMessageReq = ReorderIssueMessageReq
  { messageOrder :: [(Id IssueMessage, Int)]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets ReorderIssueMessageReq where
  hideSecrets = identity
