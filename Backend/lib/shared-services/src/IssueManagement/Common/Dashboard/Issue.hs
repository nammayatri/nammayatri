{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Time
import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueMessage
import IssueManagement.Domain.Types.Issue.IssueOption
import IssueManagement.Domain.Types.Issue.IssueReport
import IssueManagement.Domain.Types.MediaFile (MediaFile)
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.CacheFlow as Reexport
import Kernel.Types.Common
import Kernel.Types.HideSecrets as Reexport
import Kernel.Types.Id
import Servant hiding (Summary)

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data IssueEndpoint
  = IssueUpdateEndpoint
  | IssueAddCommentEndpoint
  | TicketStatusCallBackEndpoint
  | CreateIssueCategoryEndpoint
  | UpdateIssueCategoryEndpoint
  | CreateIssueOptionEndpoint
  | UpdateIssueOptionEndpoint
  | UpsertIssueMessageEndpoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord, ToSchema)

derivePersistField "IssueEndpoint"

---------------------------------------------------------
-- issue category list --------------------------------

type IssueCategoryListAPI =
  "category"
    :> Get '[JSON] IssueCategoryListRes

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

type IssueListAPI =
  "list"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "status" IssueStatus
    :> QueryParam "category" (Id IssueCategory)
    :> QueryParam "assignee" Text
    :> Get '[JSON] IssueReportListResponse

data IssueReportListResponse = IssueReportListResponse
  { issues :: [IssueReportListItem],
    summary :: Summary
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueReportListItem = IssueReportListItem
  { issueReportId :: Id IssueReport,
    issueReportShortId :: Maybe (ShortId IssueReport),
    personId :: Id Person,
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

type IssueInfoAPI =
  Capture "issueId" (Id IssueReport)
    :> "info"
    :> Get '[JSON] IssueInfoRes

type IssueInfoAPIV2 =
  "info"
    :> QueryParam "issueId" (Id IssueReport)
    :> QueryParam "issueShortId" (ShortId IssueReport)
    :> Get '[JSON] IssueInfoRes

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
  { personId :: Id Person,
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

---------------------------------------------------------
-- update issue --------------------------------------

type IssueUpdateAPI =
  Capture "issueId" (Id IssueReport)
    :> ( "update"
           :> ReqBody '[JSON] IssueUpdateReq
           :> Put '[JSON] APISuccess
       )

data IssueUpdateReq = IssueUpdateReq
  { status :: Maybe IssueStatus,
    assignee :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets IssueUpdateReq where
  hideSecrets = identity

type IssueUpdateByUserAPI =
  Capture "issueId" (Id IssueReport)
    :> ( "update"
           :> ReqBody '[JSON] IssueUpdateByUserReq
           :> Put '[JSON] APISuccess
       )

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

type IssueAddCommentAPI =
  Capture "issueId" (Id IssueReport)
    :> ( "comment"
           :> ReqBody '[JSON] IssueAddCommentReq
           :> Post '[JSON] APISuccess
       )

newtype IssueAddCommentReq = IssueAddCommentReq
  { comment :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets IssueAddCommentReq where
  hideSecrets = identity

type IssueAddCommentByUserAPI =
  Capture "issueId" (Id IssueReport)
    :> ( "comment"
           :> ReqBody '[JSON] IssueAddCommentByUserReq
           :> Post '[JSON] APISuccess
       )

data IssueAddCommentByUserReq = IssueAddCommentByUserReq
  { comment :: Text,
    userId :: Id User
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets IssueAddCommentByUserReq where
  hideSecrets = identity

type IssueFetchMediaAPI =
  "media"
    :> MandatoryQueryParam "filePath" Text
    :> Get '[JSON] Text

---------------------------------------------------------
-- Ticket Status Call Back --------------------------------------

type TicketStatusCallBackAPI =
  "kapture"
    :> "ticketStatus"
    :> ReqBody '[JSON] Value
    :> Post '[JSON] APISuccess

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

type CreateIssueCategoryAPI =
  "category"
    :> "create"
    :> ReqBody '[JSON] CreateIssueCategoryReq
    :> Post '[JSON] CreateIssueCategoryRes

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
    label :: Maybe Text
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
    showOnlyWhenUserBlocked :: Maybe Bool
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

type UpdateIssueCategoryAPI =
  "category"
    :> "update"
    :> MandatoryQueryParam "issueCategoryId" (Id IssueCategory)
    :> ReqBody '[JSON] UpdateIssueCategoryReq
    :> Post '[JSON] APISuccess

data UpdateIssueCategoryReq = UpdateIssueCategoryReq
  { category :: Maybe Text,
    logoUrl :: Maybe Text,
    priority :: Maybe Int,
    isRideRequired :: Maybe Bool,
    maxAllowedRideAge :: Maybe Seconds,
    allowedRideStatuses :: Maybe [RideStatus],
    isActive :: Maybe Bool,
    translations :: [Translation],
    label :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UpdateIssueCategoryReq where
  hideSecrets = identity

-----------------------------------------------------------
-- Create IssueOption API ------------------------------------

type CreateIssueOptionAPI =
  "option"
    :> "create"
    :> MandatoryQueryParam "issueCategoryId" (Id IssueCategory)
    :> MandatoryQueryParam "issueMessageId" (Id IssueMessage)
    :> ReqBody '[JSON] CreateIssueOptionReq
    :> Post '[JSON] CreateIssueOptionRes

newtype CreateIssueOptionRes = CreateIssueOptionRes
  { optionId :: Id IssueOption
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets CreateIssueOptionReq where
  hideSecrets = identity

-----------------------------------------------------------
-- Update IssueOption API ------------------------------------

type UpdateIssueOptionAPI =
  "option"
    :> "update"
    :> MandatoryQueryParam "issueOptionid" (Id IssueOption)
    :> ReqBody '[JSON] UpdateIssueOptionReq
    :> Post '[JSON] APISuccess

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
    showOnlyWhenUserBlocked :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UpdateIssueOptionReq where
  hideSecrets = identity

-----------------------------------------------------------
-- Upsert IssueMessage API ------------------------------------

type UpsertIssueMessageAPI =
  "message"
    :> "upsert"
    :> MultipartForm Tmp UpsertIssueMessageReq
    :> Post '[JSON] UpsertIssueMessageRes

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
