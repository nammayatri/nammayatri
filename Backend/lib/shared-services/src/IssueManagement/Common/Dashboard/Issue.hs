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
import IssueManagement.Domain.Types.Issue.IssueMessage
import IssueManagement.Domain.Types.Issue.IssueOption
import IssueManagement.Domain.Types.Issue.IssueReport
import IssueManagement.Domain.Types.MediaFile (MediaFile)
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
    maxAllowedRideAge :: Maybe Seconds,
    allowedRideStatuses :: Maybe [RideStatus]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype IssueCategoryListRes = IssueCategoryListRes
  { categories :: [IssueCategoryRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

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
    deleted :: Bool,
    category :: Text,
    assignee :: Maybe Text,
    status :: IssueStatus,
    createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- issue info --------------------------------

data IssueInfoRes = IssueInfoRes
  { issueReportId :: Id IssueReport,
    issueReportShortId :: Maybe (ShortId IssueReport),
    personDetail :: Maybe PersonDetail,
    rideId :: Maybe (Id Ride),
    chats :: Maybe [ChatDetail],
    comments :: [IssueReportCommentItem],
    category :: Text,
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
  { totalCount :: Int, --TODO add db indexes
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
      files = maybe [] (map mkFileData) req.mediaFiles
      mkFileData (IssueMessageMediaFileUploadReq filePath contType) =
        FileData "file" (T.pack filePath) contType filePath

      encodeJson :: ToJSON a => a -> Text
      encodeJson = DTE.decodeUtf8 . BL.toStrict . encode

instance HideSecrets UpsertIssueMessageReq where
  hideSecrets = identity
