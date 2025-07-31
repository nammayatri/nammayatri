{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module IGM.Types
  ( Ack (..),
    Action (..),
    ActionActorDetails (..),
    ActionDescriptor (..),
    ActorInfo (..),
    ActorInfoInfo (..),
    AdditionalDesc (..),
    Contact (..),
    Context (..),
    Location (..),
    City (..),
    Credential (..),
    Descriptor (..),
    Error (..),
    Image (..),
    Issue (..),
    IssuePost200Response (..),
    IssuePost200ResponseMessage (..),
    IssueReq (..),
    IssueReqMessage (..),
    IssueStatusReq (..),
    IssueStatusReqMessage (..),
    MediaFile (..),
    OnIssueReq (..),
    OnIssueReqMessage (..),
    OnIssueStatusReq (..),
    Person (..),
    PersonLanguagesInner (..),
    PersonSkillsInner (..),
    Refs (..),
    Resolution (..),
    ResolutionDescriptor (..),
    Schedule (..),
    Tag (..),
    TagGroup (..),
    Time (..),
    TimeRange (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, genericParseJSON, genericToJSON)
import Data.Aeson.Types (Options (..), defaultOptions)
import qualified Data.Char as Char
import Data.Data (Data)
import Data.List (lookup)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.UUID (UUID)
import GHC.Generics (Generic)

-- | Describes the ACK response
data Ack = Ack
  { -- | Describe the status of the ACK response. If schema validation passes, status is ACK else it is NACK
    ackStatus :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Ack where
  parseJSON = genericParseJSON optionsAck

instance ToJSON Ack where
  toJSON = genericToJSON optionsAck

optionsAck :: Options
optionsAck =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("ackStatus", "status")
      ]

-- |
data Action = Action
  { -- | Describes id of the action
    actionId :: Maybe Text,
    -- | Describes the reference id
    actionRefUnderscoreid :: Maybe Text,
    -- | Describes the refence type
    actionRefUnderscoretype :: Maybe Text,
    -- |
    actionDescriptor :: Maybe ActionDescriptor,
    -- | timestamp for the capturing the time an issue status was last updated
    actionUpdatedUnderscoreat :: Maybe UTCTime,
    -- | Describes the action_by
    actionActionUnderscoreby :: Maybe Text,
    -- |
    actionTags :: Maybe [TagGroup],
    -- |
    actionActorUnderscoredetails :: Maybe ActionActorDetails
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Action where
  parseJSON = genericParseJSON optionsAction

instance ToJSON Action where
  toJSON = genericToJSON optionsAction

optionsAction :: Options
optionsAction =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("actionId", "id"),
        ("actionRefUnderscoreid", "ref_id"),
        ("actionRefUnderscoretype", "ref_type"),
        ("actionDescriptor", "descriptor"),
        ("actionUpdatedUnderscoreat", "updated_at"),
        ("actionActionUnderscoreby", "action_by"),
        ("actionTags", "tags"),
        ("actionActorUnderscoredetails", "actor_details")
      ]

-- |
data ActionActorDetails = ActionActorDetails
  { -- |
    actionActorDetailsName :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ActionActorDetails where
  parseJSON = genericParseJSON optionsActionActorDetails

instance ToJSON ActionActorDetails where
  toJSON = genericToJSON optionsActionActorDetails

optionsActionActorDetails :: Options
optionsActionActorDetails =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("actionActorDetailsName", "name")
      ]

-- |
data ActionDescriptor = ActionDescriptor
  { -- |
    actionDescriptorName :: Maybe Text,
    -- |
    actionDescriptorCode :: Maybe Text,
    -- |
    actionDescriptorShortUnderscoredesc :: Maybe Text,
    -- |
    actionDescriptorLongUnderscoredesc :: Maybe Text,
    -- |
    actionDescriptorAdditionalUnderscoredesc :: Maybe AdditionalDesc,
    -- |
    actionDescriptorMedia :: Maybe [MediaFile],
    -- |
    actionDescriptorImages :: Maybe [Image]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ActionDescriptor where
  parseJSON = genericParseJSON optionsActionDescriptor

instance ToJSON ActionDescriptor where
  toJSON = genericToJSON optionsActionDescriptor

optionsActionDescriptor :: Options
optionsActionDescriptor =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("actionDescriptorName", "name"),
        ("actionDescriptorCode", "code"),
        ("actionDescriptorShortUnderscoredesc", "short_desc"),
        ("actionDescriptorLongUnderscoredesc", "long_desc"),
        ("actionDescriptorAdditionalUnderscoredesc", "additional_desc"),
        ("actionDescriptorMedia", "media"),
        ("actionDescriptorImages", "images")
      ]

-- |
data ActorInfo = ActorInfo
  { -- | Describes id of the actors that are their
    actorInfoId :: Maybe Text,
    -- | Describes the type of actor
    actorInfoType :: Maybe Text,
    -- |
    actorInfoInfo :: Maybe ActorInfoInfo
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ActorInfo where
  parseJSON = genericParseJSON optionsActorInfo

instance ToJSON ActorInfo where
  toJSON = genericToJSON optionsActorInfo

optionsActorInfo :: Options
optionsActorInfo =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("actorInfoId", "id"),
        ("actorInfoType", "type"),
        ("actorInfoInfo", "info")
      ]

-- | Describes the information about actor
data ActorInfoInfo = ActorInfoInfo
  { -- |
    actorInfoInfoPerson :: Maybe Person,
    -- |
    actorInfoInfoContact :: Maybe Contact,
    -- |
    actorInfoInfoOrg :: Maybe Value
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ActorInfoInfo where
  parseJSON = genericParseJSON optionsActorInfoInfo

instance ToJSON ActorInfoInfo where
  toJSON = genericToJSON optionsActorInfoInfo

optionsActorInfoInfo :: Options
optionsActorInfoInfo =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("actorInfoInfoPerson", "person"),
        ("actorInfoInfoContact", "contact"),
        ("actorInfoInfoOrg", "org")
      ]

-- |
data AdditionalDesc = AdditionalDesc
  { -- |
    additionalDescUrl :: Maybe Text,
    -- |
    additionalDescContentUnderscoretype :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON AdditionalDesc where
  parseJSON = genericParseJSON optionsAdditionalDesc

instance ToJSON AdditionalDesc where
  toJSON = genericToJSON optionsAdditionalDesc

optionsAdditionalDesc :: Options
optionsAdditionalDesc =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("additionalDescUrl", "url"),
        ("additionalDescContentUnderscoretype", "content_type")
      ]

-- |
data Contact = Contact
  { -- |
    contactPhone :: Maybe Text,
    -- |
    contactEmail :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Contact where
  parseJSON = genericParseJSON optionsContact

instance ToJSON Contact where
  toJSON = genericToJSON optionsContact

optionsContact :: Options
optionsContact =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("contactPhone", "phone"),
        ("contactEmail", "email")
      ]

-- | Describes a beckn message context
data Context = Context
  { -- |
    contextDomain :: Text,
    -- |
    contextCountry :: Maybe Text,
    -- |
    contextCity :: Maybe Text,
    -- |
    contextLocation :: Maybe Location,
    -- | Defines the Beckn API call. Any actions other than the enumerated actions are not supported by Beckn Protocol
    contextAction :: Text,
    -- | Version of Beckn core API specification being used
    contextCoreUnderscoreversion :: Maybe Text,
    -- | Version of Beckn core API specification being used
    contextVersion :: Maybe Text,
    -- | Unique id of the BAP. By default it is the fully qualified domain name of the BAP
    contextBapUnderscoreid :: Text,
    -- | URI of the BAP for accepting callbacks. Must have the same domain name as the bap_id
    contextBapUnderscoreuri :: Text,
    -- | Unique id of the BPP. By default it is the fully qualified domain name of the BPP
    contextBppUnderscoreid :: Maybe Text,
    -- | URI of the BPP. Must have the same domain name as the bap_id
    contextBppUnderscoreuri :: Maybe Text,
    -- | This is a unique value which persists across all API calls from search through confirm
    contextTransactionUnderscoreid :: Text,
    -- | This is a unique value which persists during a request / callback cycle
    contextMessageUnderscoreid :: Text,
    -- | Time of request generation in RFC3339 format
    contextTimestamp :: UTCTime,
    -- | The encryption public key of the sender
    contextKey :: Maybe Text,
    -- | The duration in ISO8601 format after timestamp for which this message holds valid
    contextTtl :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Context where
  parseJSON = genericParseJSON optionsContext

instance ToJSON Context where
  toJSON = genericToJSON optionsContext

optionsContext :: Options
optionsContext =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("contextDomain", "domain"),
        ("contextCountry", "country"),
        ("contextCity", "city"),
        ("contextLocation", "location"),
        ("contextAction", "action"),
        ("contextCoreUnderscoreversion", "core_version"),
        ("contextVersion", "version"),
        ("contextBapUnderscoreid", "bap_id"),
        ("contextBapUnderscoreuri", "bap_uri"),
        ("contextBppUnderscoreid", "bpp_id"),
        ("contextBppUnderscoreuri", "bpp_uri"),
        ("contextTransactionUnderscoreid", "transaction_id"),
        ("contextMessageUnderscoreid", "message_id"),
        ("contextTimestamp", "timestamp"),
        ("contextKey", "key"),
        ("contextTtl", "ttl")
      ]

-- |
data Location = Location
  { -- |
    contextLocationCity :: Maybe City,
    -- |
    contextLocationCountry :: Maybe City
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Location where
  parseJSON = genericParseJSON optionsContextLocation

instance ToJSON Location where
  toJSON = genericToJSON optionsContextLocation

optionsContextLocation :: Options
optionsContextLocation =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("contextLocationCity", "city"),
        ("contextLocationCountry", "country")
      ]

-- |
data City = City
  { -- | defines code for city
    contextLocationCityCode :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON City where
  parseJSON = genericParseJSON optionsContextLocationCity

instance ToJSON City where
  toJSON = genericToJSON optionsContextLocationCity

optionsContextLocationCity :: Options
optionsContextLocationCity =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("contextLocationCityCode", "code")
      ]

-- | Describes a credential of an entity - Person or Organization
data Credential = Credential
  { -- |
    credentialId :: Maybe Text,
    -- |
    credentialType :: Maybe Text,
    -- | URL of the credential
    credentialUrl :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Credential where
  parseJSON = genericParseJSON optionsCredential

instance ToJSON Credential where
  toJSON = genericToJSON optionsCredential

optionsCredential :: Options
optionsCredential =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("credentialId", "id"),
        ("credentialType", "type"),
        ("credentialUrl", "url")
      ]

-- | Physical description of something.
data Descriptor = Descriptor
  { -- |
    descriptorName :: Maybe Text,
    -- |
    descriptorCode :: Maybe Text,
    -- |
    descriptorShortUnderscoredesc :: Maybe Text,
    -- |
    descriptorLongUnderscoredesc :: Maybe Text,
    -- |
    descriptorAdditionalUnderscoredesc :: Maybe AdditionalDesc,
    -- |
    descriptorMedia :: Maybe [MediaFile],
    -- |
    descriptorImages :: Maybe [Image]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Descriptor where
  parseJSON = genericParseJSON optionsDescriptor

instance ToJSON Descriptor where
  toJSON = genericToJSON optionsDescriptor

optionsDescriptor :: Options
optionsDescriptor =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("descriptorName", "name"),
        ("descriptorCode", "code"),
        ("descriptorShortUnderscoredesc", "short_desc"),
        ("descriptorLongUnderscoredesc", "long_desc"),
        ("descriptorAdditionalUnderscoredesc", "additional_desc"),
        ("descriptorMedia", "media"),
        ("descriptorImages", "images")
      ]

-- | Describes an error object
data Error = Error
  { -- |
    errorType :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Error where
  parseJSON = genericParseJSON optionsError

instance ToJSON Error where
  toJSON = genericToJSON optionsError

optionsError :: Options
optionsError =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("errorType", "type")
      ]

-- | Describes an image
data Image = Image
  { -- | URL to the image. This can be a data url or an remote url
    imageUrl :: Maybe Text,
    -- | The size of the image. The network policy can define the default dimensions of each type
    imageSizeUnderscoretype :: Maybe Text,
    -- | Width of the image in pixels
    imageWidth :: Maybe Text,
    -- | Height of the image in pixels
    imageHeight :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Image where
  parseJSON = genericParseJSON optionsImage

instance ToJSON Image where
  toJSON = genericToJSON optionsImage

optionsImage :: Options
optionsImage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("imageUrl", "url"),
        ("imageSizeUnderscoretype", "size_type"),
        ("imageWidth", "width"),
        ("imageHeight", "height")
      ]

-- | *  Describes the Issue/complaint raised for a particular order, transaction or fulfillment or item for which an issue is faced by the buyer. * It also describes the Issue/complaint raised by the network participants to its conuter party or casacaded counterparty apps as and when applicable * Describes the issue when it is escalated as a Grievance or a Dispute *  supplementary Information  Describes and details evidence of the information about the issue . When the complainant or respondent seeks an issue update the addtional information about the update are populated as part of the supplementary information.   the supplementary information section , details information including but not limited to any of the following or more:     * comments from the complaintant/ respondent for seeking more information about the issue     * comments from the complaintant/ respondent providing the update on the issue with more information on the issue     * Picutres providing further information about the issue     * documents attachment/links providing further information about the issue etc
data Issue = Issue
  { -- | * Network issue identifier is an unique number assigned to any complaint by the interfacing application at the source.  * **During the life cycle of a complaint, this number will not change and will be communicated to counterparty application.**  * Any subsequent issues created with the interfacing,counterpary or casacaded counter party apps are responsible for providing the correalted and relevant network-issue-id which helps all this inteacting systems to traverse back to the main issue.
    issueId :: Maybe Text,
    -- | Reference to a complaint for which a status is raised.
    issueIssueUnderscoreid :: Maybe Text,
    -- |
    issueExpectedUnderscoreresponseUnderscoretime :: Maybe Time,
    -- |
    issueExpectedUnderscoreresolutionUnderscoretime :: Maybe Time,
    -- | ### Statues   1. Open : indicating that the issue is opened and further action is yet to be taken on the issue   2. Closed : indicating that the issue is closed and no further action/update/info/resolution action/status change are expected/accepted on this issue.
    issueStatus :: Maybe Text,
    -- | indicating the issue and grievance level
    issueLevel :: Maybe Text,
    -- | timestamp for the creation of the issue
    issueCreatedUnderscoreat :: Maybe UTCTime,
    -- | timestamp for the capturing the time an issue was last updated
    issueUpdatedUnderscoreat :: Maybe UTCTime,
    -- | Describesabout the source id
    issueSourceUnderscoreid :: Maybe Text,
    -- | Describe about the complainant_id
    issueComplainantUnderscoreid :: Maybe Text,
    -- | Describes about the respondent_id
    issueRespondentUnderscoreids :: Maybe [Text],
    -- | Describes about the respondent_id
    issueResolverUnderscoreids :: Maybe [Text],
    -- | Describes the last action id
    issueLastUnderscoreactionUnderscoreid :: Maybe Text,
    -- | state the description of the action
    issueActions :: Maybe [Action],
    -- |
    issueDescriptor :: Maybe Descriptor,
    -- | state the description of the actor that are involve
    issueActors :: Maybe [ActorInfo],
    -- |
    issueResolutions :: Maybe [Resolution],
    -- | state the reference of the oreference
    issueRefs :: Maybe [Refs],
    -- |
    issueTags :: Maybe [TagGroup]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Issue where
  parseJSON = genericParseJSON optionsIssue

instance ToJSON Issue where
  toJSON = genericToJSON optionsIssue

optionsIssue :: Options
optionsIssue =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("issueId", "id"),
        ("issueIssueUnderscoreid", "issue_id"),
        ("issueExpectedUnderscoreresponseUnderscoretime", "expected_response_time"),
        ("issueExpectedUnderscoreresolutionUnderscoretime", "expected_resolution_time"),
        ("issueStatus", "status"),
        ("issueLevel", "level"),
        ("issueCreatedUnderscoreat", "created_at"),
        ("issueUpdatedUnderscoreat", "updated_at"),
        ("issueSourceUnderscoreid", "source_id"),
        ("issueComplainantUnderscoreid", "complainant_id"),
        ("issueRespondentUnderscoreids", "respondent_ids"),
        ("issueResolverUnderscoreids", "resolver_ids"),
        ("issueLastUnderscoreactionUnderscoreid", "last_action_id"),
        ("issueActions", "actions"),
        ("issueDescriptor", "descriptor"),
        ("issueActors", "actors"),
        ("issueResolutions", "resolutions"),
        ("issueRefs", "refs"),
        ("issueTags", "tags")
      ]

-- |
data IssuePost200Response = IssuePost200Response
  { -- |
    issuePost200ResponseMessage :: IssuePost200ResponseMessage,
    -- |
    issuePost200ResponseError :: Maybe Error
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssuePost200Response where
  parseJSON = genericParseJSON optionsIssuePost200Response

instance ToJSON IssuePost200Response where
  toJSON = genericToJSON optionsIssuePost200Response

optionsIssuePost200Response :: Options
optionsIssuePost200Response =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("issuePost200ResponseMessage", "message"),
        ("issuePost200ResponseError", "error")
      ]

-- |
data IssuePost200ResponseMessage = IssuePost200ResponseMessage
  { -- |
    issuePost200ResponseMessageAck :: Ack
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssuePost200ResponseMessage where
  parseJSON = genericParseJSON optionsIssuePost200ResponseMessage

instance ToJSON IssuePost200ResponseMessage where
  toJSON = genericToJSON optionsIssuePost200ResponseMessage

optionsIssuePost200ResponseMessage :: Options
optionsIssuePost200ResponseMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("issuePost200ResponseMessageAck", "ack")
      ]

-- |
data IssueReq = IssueReq
  { -- |
    issueReqContext :: Context,
    -- |
    issueReqMessage :: IssueReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssueReq where
  parseJSON = genericParseJSON optionsIssueReq

instance ToJSON IssueReq where
  toJSON = genericToJSON optionsIssueReq

optionsIssueReq :: Options
optionsIssueReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("issueReqContext", "context"),
        ("issueReqMessage", "message")
      ]

-- |
data IssueReqMessage = IssueReqMessage
  { -- |
    issueReqMessageIssue :: Maybe Issue
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssueReqMessage where
  parseJSON = genericParseJSON optionsIssueReqMessage

instance ToJSON IssueReqMessage where
  toJSON = genericToJSON optionsIssueReqMessage

optionsIssueReqMessage :: Options
optionsIssueReqMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("issueReqMessageIssue", "issue")
      ]

-- |
data IssueStatusReq = IssueStatusReq
  { -- |
    issueStatusReqContext :: Context,
    -- |
    issueStatusReqMessage :: IssueStatusReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssueStatusReq where
  parseJSON = genericParseJSON optionsIssueStatusReq

instance ToJSON IssueStatusReq where
  toJSON = genericToJSON optionsIssueStatusReq

optionsIssueStatusReq :: Options
optionsIssueStatusReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("issueStatusReqContext", "context"),
        ("issueStatusReqMessage", "message")
      ]

-- |
data IssueStatusReqMessage = IssueStatusReqMessage
  { -- | Network issue identifier is a unique number assigned to a complaint by the interfacing application
    issueStatusReqMessageIssueUnderscoreid :: Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssueStatusReqMessage where
  parseJSON = genericParseJSON optionsIssueStatusReqMessage

instance ToJSON IssueStatusReqMessage where
  toJSON = genericToJSON optionsIssueStatusReqMessage

optionsIssueStatusReqMessage :: Options
optionsIssueStatusReqMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("issueStatusReqMessageIssueUnderscoreid", "issue_id")
      ]

-- | This object contains a url to a media file.
data MediaFile = MediaFile
  { -- | indicates the nature and format of the document, file, or assortment of bytes. MIME types are defined and standardized in IETF's RFC 6838
    mediaFileMimetype :: Maybe Text,
    -- | The URL of the file
    mediaFileUrl :: Maybe Text,
    -- | The digital signature of the file signed by the sender
    mediaFileSignature :: Maybe Text,
    -- | The signing algorithm used by the sender
    mediaFileDsa :: Maybe Text,
    -- |
    mediaFileTime :: Maybe Time
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON MediaFile where
  parseJSON = genericParseJSON optionsMediaFile

instance ToJSON MediaFile where
  toJSON = genericToJSON optionsMediaFile

optionsMediaFile :: Options
optionsMediaFile =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("mediaFileMimetype", "mimetype"),
        ("mediaFileUrl", "url"),
        ("mediaFileSignature", "signature"),
        ("mediaFileDsa", "dsa"),
        ("mediaFileTime", "time")
      ]

-- |
data OnIssueReq = OnIssueReq
  { -- |
    onIssueReqContext :: Context,
    -- |
    onIssueReqMessage :: OnIssueReqMessage,
    -- |
    onIssueReqError :: Maybe Error
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OnIssueReq where
  parseJSON = genericParseJSON optionsOnIssueReq

instance ToJSON OnIssueReq where
  toJSON = genericToJSON optionsOnIssueReq

optionsOnIssueReq :: Options
optionsOnIssueReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("onIssueReqContext", "context"),
        ("onIssueReqMessage", "message"),
        ("onIssueReqError", "error")
      ]

-- |
data OnIssueReqMessage = OnIssueReqMessage
  { -- |
    onIssueReqMessageIssue :: Issue
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OnIssueReqMessage where
  parseJSON = genericParseJSON optionsOnIssueReqMessage

instance ToJSON OnIssueReqMessage where
  toJSON = genericToJSON optionsOnIssueReqMessage

optionsOnIssueReqMessage :: Options
optionsOnIssueReqMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("onIssueReqMessageIssue", "issue")
      ]

-- |
data OnIssueStatusReq = OnIssueStatusReq
  { -- |
    onIssueStatusReqContext :: Context,
    -- |
    onIssueStatusReqMessage :: OnIssueReqMessage,
    -- |
    onIssueStatusReqError :: Maybe Error
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OnIssueStatusReq where
  parseJSON = genericParseJSON optionsOnIssueStatusReq

instance ToJSON OnIssueStatusReq where
  toJSON = genericToJSON optionsOnIssueStatusReq

optionsOnIssueStatusReq :: Options
optionsOnIssueStatusReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("onIssueStatusReqContext", "context"),
        ("onIssueStatusReqMessage", "message"),
        ("onIssueStatusReqError", "error")
      ]

-- | Describes a person as any individual
data Person = Person
  { -- | Describes the identity of the person
    personId :: Maybe Text,
    -- | Profile url of the person
    personUrl :: Maybe Text,
    -- | the name of the person
    personName :: Maybe Text,
    -- |
    personImage :: Maybe Image,
    -- | Age of the person
    personAge :: Maybe Text,
    -- | Date of birth of the person
    personDob :: Maybe Day,
    -- | Gender of something, typically a Person, but possibly also fictional characters, animals, etc. While Male and Female may be used, text strings are also acceptable for people who do not identify as a binary gender.Allowed values for this field can be published in the network policy
    personGender :: Maybe Text,
    -- |
    personCreds :: Maybe [Credential],
    -- |
    personLanguages :: Maybe [PersonLanguagesInner],
    -- |
    personSkills :: Maybe [PersonSkillsInner],
    -- |
    personTags :: Maybe [TagGroup]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Person where
  parseJSON = genericParseJSON optionsPerson

instance ToJSON Person where
  toJSON = genericToJSON optionsPerson

optionsPerson :: Options
optionsPerson =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personId", "id"),
        ("personUrl", "url"),
        ("personName", "name"),
        ("personImage", "image"),
        ("personAge", "age"),
        ("personDob", "dob"),
        ("personGender", "gender"),
        ("personCreds", "creds"),
        ("personLanguages", "languages"),
        ("personSkills", "skills"),
        ("personTags", "tags")
      ]

-- | Describes a language known to the person.
data PersonLanguagesInner = PersonLanguagesInner
  { -- |
    personLanguagesInnerCode :: Maybe Text,
    -- |
    personLanguagesInnerName :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON PersonLanguagesInner where
  parseJSON = genericParseJSON optionsPersonLanguagesInner

instance ToJSON PersonLanguagesInner where
  toJSON = genericToJSON optionsPersonLanguagesInner

optionsPersonLanguagesInner :: Options
optionsPersonLanguagesInner =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personLanguagesInnerCode", "code"),
        ("personLanguagesInnerName", "name")
      ]

-- | Describes a skill of the person.
data PersonSkillsInner = PersonSkillsInner
  { -- |
    personSkillsInnerCode :: Maybe Text,
    -- |
    personSkillsInnerName :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON PersonSkillsInner where
  parseJSON = genericParseJSON optionsPersonSkillsInner

instance ToJSON PersonSkillsInner where
  toJSON = genericToJSON optionsPersonSkillsInner

optionsPersonSkillsInner :: Options
optionsPersonSkillsInner =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personSkillsInnerCode", "code"),
        ("personSkillsInnerName", "name")
      ]

-- |
data Refs = Refs
  { -- | tells abot ref id
    refsRefUnderscoreid :: Maybe Text,
    -- | tells abot ref type
    refsRefUnderscoretype :: Maybe Text,
    -- | tells abot ref details
    refsTags :: Maybe [TagGroup]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Refs where
  parseJSON = genericParseJSON optionsRefs

instance ToJSON Refs where
  toJSON = genericToJSON optionsRefs

optionsRefs :: Options
optionsRefs =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("refsRefUnderscoreid", "ref_id"),
        ("refsRefUnderscoretype", "ref_type"),
        ("refsTags", "tags")
      ]

-- |
data Resolution = Resolution
  { -- | Describes id of the action
    resolutionId :: Maybe Text,
    -- | Describes the reference id
    resolutionRefUnderscoreid :: Maybe Text,
    -- | Describes the refence type
    resolutionRefUnderscoretype :: Maybe Text,
    -- |
    resolutionDescriptor :: Maybe ResolutionDescriptor,
    -- | timestamp for the capturing the time an issue status was last updated
    resolutionUpdatedUnderscoreat :: Maybe UTCTime,
    -- | Describes the action_by
    resolutionProposedUnderscoreby :: Maybe Text,
    -- |
    resolutionTags :: Maybe [TagGroup]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Resolution where
  parseJSON = genericParseJSON optionsResolution

instance ToJSON Resolution where
  toJSON = genericToJSON optionsResolution

optionsResolution :: Options
optionsResolution =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("resolutionId", "id"),
        ("resolutionRefUnderscoreid", "ref_id"),
        ("resolutionRefUnderscoretype", "ref_type"),
        ("resolutionDescriptor", "descriptor"),
        ("resolutionUpdatedUnderscoreat", "updated_at"),
        ("resolutionProposedUnderscoreby", "proposed_by"),
        ("resolutionTags", "tags")
      ]

-- |
data ResolutionDescriptor = ResolutionDescriptor
  { -- |
    resolutionDescriptorName :: Maybe Text,
    -- |
    resolutionDescriptorCode :: Maybe Text,
    -- |
    resolutionDescriptorShortUnderscoredesc :: Maybe Text,
    -- |
    resolutionDescriptorLongUnderscoredesc :: Maybe Text,
    -- |
    resolutionDescriptorAdditionalUnderscoredesc :: Maybe AdditionalDesc,
    -- |
    resolutionDescriptorMedia :: Maybe [MediaFile],
    -- |
    resolutionDescriptorImages :: Maybe [Image]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ResolutionDescriptor where
  parseJSON = genericParseJSON optionsResolutionDescriptor

instance ToJSON ResolutionDescriptor where
  toJSON = genericToJSON optionsResolutionDescriptor

optionsResolutionDescriptor :: Options
optionsResolutionDescriptor =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("resolutionDescriptorName", "name"),
        ("resolutionDescriptorCode", "code"),
        ("resolutionDescriptorShortUnderscoredesc", "short_desc"),
        ("resolutionDescriptorLongUnderscoredesc", "long_desc"),
        ("resolutionDescriptorAdditionalUnderscoredesc", "additional_desc"),
        ("resolutionDescriptorMedia", "media"),
        ("resolutionDescriptorImages", "images")
      ]

-- | Describes schedule as a repeating time period used to describe a regularly recurring event. At a minimum a schedule will specify frequency which describes the interval between occurrences of the event. Additional information can be provided to specify the schedule more precisely. This includes identifying the timestamps(s) of when the event will take place. Schedules may also have holidays to exclude a specific day from the schedule.&lt;br&gt;This has properties like frequency, holidays, times
data Schedule = Schedule
  { -- | Describes duration as per ISO8601 format
    scheduleFrequency :: Maybe Text,
    -- |
    scheduleHolidays :: Maybe [UTCTime],
    -- |
    scheduleTimes :: Maybe [UTCTime]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Schedule where
  parseJSON = genericParseJSON optionsSchedule

instance ToJSON Schedule where
  toJSON = genericToJSON optionsSchedule

optionsSchedule :: Options
optionsSchedule =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("scheduleFrequency", "frequency"),
        ("scheduleHolidays", "holidays"),
        ("scheduleTimes", "times")
      ]

-- | Describes a tag. This is used to contain extended metadata. This object can be added as a property to any schema to describe extended attributes. For BAPs, tags can be sent during search to optimize and filter search results. BPPs can use tags to index their catalog to allow better search functionality. Tags are sent by the BPP as part of the catalog response in the &#x60;on_search&#x60; callback. Tags are also meant for display purposes. Upon receiving a tag, BAPs are meant to render them as name-value pairs. This is particularly useful when rendering tabular information about a product or service.
data Tag = Tag
  { -- | Description of the Tag, can be used to store detailed information.
    tagDescriptor :: Maybe Descriptor,
    -- | The value of the tag. This set by the BPP and rendered as-is by the BAP.
    tagValue :: Maybe Text,
    -- | This value indicates if the tag is intended for display purposes. If set to `true`, then this tag must be displayed. If it is set to `false`, it should not be displayed. This value can override the group display value.
    tagDisplay :: Maybe Bool
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Tag where
  parseJSON = genericParseJSON optionsTag

instance ToJSON Tag where
  toJSON = genericToJSON optionsTag

optionsTag :: Options
optionsTag =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tagDescriptor", "descriptor"),
        ("tagValue", "value"),
        ("tagDisplay", "display")
      ]

-- | A collection of tag objects with group level attributes. For detailed documentation on the Tags and Tag Groups schema go to https://github.com/beckn/protocol-specifications/discussions/316
data TagGroup = TagGroup
  { -- | Indicates the display properties of the tag group. If display is set to false, then the group will not be displayed. If it is set to true, it should be displayed. However, group-level display properties can be overriden by individual tag-level display property. As this schema is purely for catalog display purposes, it is not recommended to send this value during search.
    tagGroupDisplay :: Maybe Bool,
    -- | Description of the TagGroup, can be used to store detailed information.
    tagGroupDescriptor :: Maybe Descriptor,
    -- | An array of Tag objects listed under this group. This property can be set by BAPs during search to narrow the `search` and achieve more relevant results. When received during `on_search`, BAPs must render this list under the heading described by the `name` property of this schema.
    tagGroupList :: Maybe [Tag]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON TagGroup where
  parseJSON = genericParseJSON optionsTagGroup

instance ToJSON TagGroup where
  toJSON = genericToJSON optionsTagGroup

optionsTagGroup :: Options
optionsTagGroup =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("tagGroupDisplay", "display"),
        ("tagGroupDescriptor", "descriptor"),
        ("tagGroupList", "list")
      ]

-- | Describes time in its various forms. It can be a single point in time; duration; or a structured timetable of operations
data Time = Time
  { -- |
    timeLabel :: Maybe Text,
    -- |
    timeTimestamp :: Maybe UTCTime,
    -- | Describes duration as per ISO8601 format
    timeDuration :: Maybe Text,
    -- |
    timeRange :: Maybe TimeRange,
    -- | comma separated values representing days of the week
    timeDays :: Maybe Text,
    -- |
    timeSchedule :: Maybe Schedule
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Time where
  parseJSON = genericParseJSON optionsTime

instance ToJSON Time where
  toJSON = genericToJSON optionsTime

optionsTime :: Options
optionsTime =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("timeLabel", "label"),
        ("timeTimestamp", "timestamp"),
        ("timeDuration", "duration"),
        ("timeRange", "range"),
        ("timeDays", "days"),
        ("timeSchedule", "schedule")
      ]

-- |
data TimeRange = TimeRange
  { -- |
    timeRangeStart :: Maybe UTCTime,
    -- |
    timeRangeEnd :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON TimeRange where
  parseJSON = genericParseJSON optionsTimeRange

instance ToJSON TimeRange where
  toJSON = genericToJSON optionsTimeRange

optionsTimeRange :: Options
optionsTimeRange =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("timeRangeStart", "start"),
        ("timeRangeEnd", "end")
      ]
