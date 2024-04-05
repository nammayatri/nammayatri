{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module IGM.Types
  ( Ack (..),
    AckMessage (..),
    AckResponse (..),
    Complainant (..),
    ComplainantAction (..),
    ComplainantPerson (..),
    Descriptor (..),
    Error (..),
    Fulfillment (..),
    GRO (..),
    GROContact (..),
    Issue (..),
    IssueActions (..),
    IssueCategory (..),
    IssueDescription (..),
    IssueExpectedResolutionTime (..),
    IssuePost200Response (..),
    IssuePost200ResponseError (..),
    IssuePost200ResponseMessage (..),
    IssuePost200ResponseMessageAck (..),
    IssueReq (..),
    IssueReqContext (..),
    IssueReqMessage (..),
    IssueResolution (..),
    IssueSource (..),
    IssueStatusReq (..),
    IssueStatusReqMessage (..),
    IssueSubCategory (..),
    Item (..),
    OnIssueReq (..),
    OnIssueReqMessage (..),
    OnIssueStatusReq (..),
    OrderDetails (..),
    Organization (..),
    OrganizationOrg (..),
    ResolutionProvider (..),
    ResolutionProviderRespondentInfo (..),
    ResolutionSupport (..),
    RespondentAction (..),
    Tag (..),
    TagGroup (..),
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
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Prelude

-- | Describes the acknowledgement sent in response to an API call. If the implementation uses HTTP/S, then Ack must be returned in the same session. Every API call to a BPP must be responded to with an Ack whether the BPP intends to respond with a callback or not. This has one property called &#x60;status&#x60; that indicates the status of the Acknowledgement.
data Ack = Ack
  { -- | The status of the acknowledgement. If the request passes the validation criteria of the BPP, then this is set to ACK. If a BPP responds with status = `ACK` to a request, it is required to respond with a callback. If the request fails the validation criteria, then this is set to NACK. Additionally, if a BPP does not intend to respond with a callback even after the request meets the validation criteria, it should set this value to `NACK`.
    ackStatus :: Maybe Text,
    -- | A list of tags containing any additional information sent along with the Acknowledgement.
    ackTags :: Maybe [TagGroup]
  }
  deriving (Show, Eq, Generic, Data, Read)

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
      [ ("ackStatus", "status"),
        ("ackTags", "tags")
      ]

-- |
data AckMessage = AckMessage
  { -- |
    ackMessageAck :: Ack
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON AckMessage where
  parseJSON = genericParseJSON optionsAckMessage

instance ToJSON AckMessage where
  toJSON = genericToJSON optionsAckMessage

optionsAckMessage :: Options
optionsAckMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("ackMessageAck", "ack")
      ]

-- |
data AckResponse = AckResponse
  { -- |
    ackResponseError :: Maybe Error,
    -- |
    ackResponseMessage :: AckMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON AckResponse where
  parseJSON = genericParseJSON optionsAckResponse

instance ToJSON AckResponse where
  toJSON = genericToJSON optionsAckResponse

optionsAckResponse :: Options
optionsAckResponse =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("ackResponseError", "error"),
        ("ackResponseMessage", "message")
      ]

-- | - Describes an entity that raises a complaint with an interfacing app.  - The complainant may be an end user (buyer or seller) or a network participant (buyer app/ seller app/ logistic services  provider app)
data Complainant = Complainant
  { -- |
    complainantContact :: Value,
    -- |
    complainantPerson :: Maybe ComplainantPerson
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Complainant where
  parseJSON = genericParseJSON optionsComplainant

instance ToJSON Complainant where
  toJSON = genericToJSON optionsComplainant

optionsComplainant :: Options
optionsComplainant =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("complainantContact", "contact"),
        ("complainantPerson", "person")
      ]

-- |
data ComplainantAction = ComplainantAction
  { -- | Describes the action taken by the complainant who has raised the issue.  the value should be populated when the complainant has taken any of the below actions to anotate the action taken. the actions can be,  ### Actions 1. Complainant can ```open``` the issue if he isn't satisfied with the products or services he has purchased or used  2. Complainant can ```escalate``` the issue if he isn't satisfied with the issue resolution 3. Complainant can ```close``` the issue if he doesn't expect any more additonal action on the issue and he is satisfied with the resolution/actions taken on the issue till the time of closure of the issue.
    complainantActionComplainantAction :: Maybe Text,
    -- | details of the remarks when the issue status is changed, can be captured when the complainant triggers an action
    complainantActionShortDesc :: Maybe Text,
    -- | timestamp for the capturing the time an issue status was last updated
    complainantActionUpdatedAt :: Maybe UTCTime,
    -- |
    complainantActionUpdatedBy :: Maybe Organization
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ComplainantAction where
  parseJSON = genericParseJSON optionsComplainantAction

instance ToJSON ComplainantAction where
  toJSON = genericToJSON optionsComplainantAction

optionsComplainantAction :: Options
optionsComplainantAction =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("complainantActionComplainantAction", "complainant_action"),
        ("complainantActionShortDesc", "short_desc"),
        ("complainantActionUpdatedAt", "updated_at"),
        ("complainantActionUpdatedBy", "updated_by")
      ]

-- | Describes a person.
data ComplainantPerson = ComplainantPerson
  { -- | Describes the name of a person in format: ./{given_name}/{honorific_prefix}/{first_name}/{middle_name}/{last_name}/{honorific_suffix}
    complainantPersonName :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ComplainantPerson where
  parseJSON = genericParseJSON optionsComplainantPerson

instance ToJSON ComplainantPerson where
  toJSON = genericToJSON optionsComplainantPerson

optionsComplainantPerson :: Options
optionsComplainantPerson =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("complainantPersonName", "name")
      ]

-- | Physical description of something.
data Descriptor = Descriptor
  { -- |
    descriptorCode :: Maybe Text,
    -- |
    descriptorName :: Maybe Text,
    -- |
    descriptorShortDesc :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data, Read)

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
      [ ("descriptorCode", "code"),
        ("descriptorName", "name"),
        ("descriptorShortDesc", "short_desc")
      ]

-- | Describes an error object that is returned by a BAP, BPP or BG as a response or callback to an action by another network participant. This object is sent when any request received by a network participant is unacceptable. This object can be sent either during Ack or with the callback.
data Error = Error
  { -- | Standard error code. For full list of error codes, refer to docs/protocol-drafts/BECKN-005-ERROR-CODES-DRAFT-01.md of this repo\"
    errorCode :: Maybe Text,
    -- | Human readable message describing the error. Used mainly for logging. Not recommended to be shown to the user.
    errorMessage :: Maybe Text,
    -- | Path to json schema generating the error. Used only during json schema validation errors
    errorPaths :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data, Read)

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
      [ ("errorCode", "code"),
        ("errorMessage", "message"),
        ("errorPaths", "paths")
      ]

-- |
data Fulfillment = Fulfillment
  { -- |
    fulfillmentId :: Maybe Text,
    -- |
    fulfillmentState :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Fulfillment where
  parseJSON = genericParseJSON optionsFulfillment

instance ToJSON Fulfillment where
  toJSON = genericToJSON optionsFulfillment

optionsFulfillment :: Options
optionsFulfillment =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fulfillmentId", "id"),
        ("fulfillmentState", "state")
      ]

-- | Grievance Redressal Officer is appointed by each network participant (buyer app, seller app, logistic provider app) for consumer grievance redressal Details of the GRO appointed by the resolution provider to support in issue resolution . This is a required object if and when the issue type is \&quot;Grievance\&quot;
data GRO = GRO
  { -- |
    gROContact :: Maybe GROContact,
    -- | Indicates the system through which the GRO is assigned to this issue.  ### TYPES     1. Intefacing NP GRO : indicating the GRO is assigned by the NP where or through which the issue is raised     2. Transaction Counterparty NP GRO : indicating the GRO is assigned by the first level of counterparty network provider with which the interfacing NP is interacting with to get an issue resolution.     3. Cascaded Counterparty NP GRO : indicating the GRO is assigned by the second or any further downstream levels of counterparty network provider, with which the NP is interacting with, to get an issue resolution.
    gROGroType :: Text,
    -- |
    gROPerson :: Maybe ComplainantPerson
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON GRO where
  parseJSON = genericParseJSON optionsGRO

instance ToJSON GRO where
  toJSON = genericToJSON optionsGRO

optionsGRO :: Options
optionsGRO =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("gROContact", "contact"),
        ("gROGroType", "gro_type"),
        ("gROPerson", "person")
      ]

-- |
data GROContact = GROContact
  { -- |
    gROContactEmail :: Maybe Text,
    -- |
    gROContactPhone :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON GROContact where
  parseJSON = genericParseJSON optionsGROContact

instance ToJSON GROContact where
  toJSON = genericToJSON optionsGROContact

optionsGROContact :: Options
optionsGROContact =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("gROContactEmail", "email"),
        ("gROContactPhone", "phone")
      ]

-- | *  Describes the Issue/complaint raised for a particular order, transaction or fulfillment or item for which an issue is faced by the buyer. * It also describes the Issue/complaint raised by the network participants to its conuter party or casacaded counterparty apps as and when applicable * Describes the issue when it is escalated as a Grievance or a Dispute *  supplementary Information  Describes and details evidence of the information about the issue . When the complainant or respondent seeks an issue update the addtional information about the update are populated as part of the supplementary information.    the supplementary information section , details information including but not limited to any of the following or more:      * comments from the complaintant/ respondent for seeking more information about the issue     * comments from the complaintant/ respondent providing the update on the issue with more information on the issue      * Picutres providing further information about the issue     * documents attachment/links providing further information about the issue etc
data Issue = Issue
  { -- |
    issueCategory :: Maybe IssueCategory,
    -- |
    issueComplainantInfo :: Maybe Complainant,
    -- | timestamp for the creation of the issue
    issueCreatedAt :: UTCTime,
    -- |
    issueDescription :: Maybe IssueDescription,
    -- |
    issueExpectedResolutionTime :: Maybe IssueExpectedResolutionTime,
    -- |
    issueExpectedResponseTime :: Maybe IssueExpectedResolutionTime,
    -- | * Network issue identifier is an unique number assigned to any complaint by the interfacing application at the source.  * **During the life cycle of a complaint, this number will not change and will be communicated to counterparty application.**  * Any subsequent issues created with the interfacing,counterpary or casacaded counter party apps are responsible for providing the correalted and relevant network-issue-id which helps all this inteacting systems to traverse back to the main issue.
    issueId :: Text,
    -- |
    issueIssueActions :: Maybe IssueActions,
    -- | identifies the type of the issue raised * the complainant being raised can be a regular ```Issue```,  * which then subsequently can be escalated to be a ```Grievance```  * which may be further escalated to be a ```Dispute```
    issueIssueType :: Maybe Text,
    -- |
    issueOrderDetails :: Maybe OrderDetails,
    -- |
    issueResolution :: Maybe IssueResolution,
    -- |
    issueResolutionProvider :: Maybe ResolutionProvider,
    -- |
    issueSource :: Maybe IssueSource,
    -- | ### Statues   1. Open : indicating that the issue is opened and further action is yet to be taken on the issue   2. Closed : indicating that the issue is closed and no further action/update/info/resolution action/status change are expected/accepted on this issue.
    issueStatus :: Maybe Text,
    -- |
    issueSubCategory :: Maybe IssueSubCategory,
    -- | timestamp for the capturing the time an issue was last updated
    issueUpdatedAt :: UTCTime
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
      [ ("issueCategory", "category"),
        ("issueComplainantInfo", "complainant_info"),
        ("issueCreatedAt", "created_at"),
        ("issueDescription", "description"),
        ("issueExpectedResolutionTime", "expected_resolution_time"),
        ("issueExpectedResponseTime", "expected_response_time"),
        ("issueId", "id"),
        ("issueIssueActions", "issue_actions"),
        ("issueIssueType", "issue_type"),
        ("issueOrderDetails", "order_details"),
        ("issueResolution", "resolution"),
        ("issueResolutionProvider", "resolution_provider"),
        ("issueSource", "source"),
        ("issueStatus", "status"),
        ("issueSubCategory", "sub_category"),
        ("issueUpdatedAt", "updated_at")
      ]

-- | Status of the issue updated by respondent or the complainant The issue may attain different status based on its lifecycle. TODO add info of enums in here and in issue-source
data IssueActions = IssueActions
  { -- |
    issueActionsComplainantActions :: Maybe [ComplainantAction],
    -- |
    issueActionsRespondentActions :: Maybe [RespondentAction]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssueActions where
  parseJSON = genericParseJSON optionsIssueActions

instance ToJSON IssueActions where
  toJSON = genericToJSON optionsIssueActions

optionsIssueActions :: Options
optionsIssueActions =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("issueActionsComplainantActions", "complainant_actions"),
        ("issueActionsRespondentActions", "respondent_actions")
      ]

-- | Identifies what is the category of this issue ### CATEGORIES: 1. Order : Indicactes that the issue is related to  Order. 2. Transaction : Indicactes that the issue is related to  Transaction. 3. Fulfillment : Indicactes that the issue is related to  Fulfillment. 4. Product : Indicactes that the issue is related to  Product. 5. Product Service : Indicactes that the issue is related to  Product Service. 6. Service : Indicactes that the issue is related to  Service. 7. Payment : Indicactes that the issue is related to  Payments.
data IssueCategory = IssueCategory
  {
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssueCategory where
  parseJSON = genericParseJSON optionsIssueCategory

instance ToJSON IssueCategory where
  toJSON = genericToJSON optionsIssueCategory

optionsIssueCategory :: Options
optionsIssueCategory =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      []

-- | Describes the description of a real-world object.
data IssueDescription = IssueDescription
  { -- |
    issueDescriptionLongDesc :: Maybe Text,
    -- |
    issueDescriptionShortDesc :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssueDescription where
  parseJSON = genericParseJSON optionsIssueDescription

instance ToJSON IssueDescription where
  toJSON = genericToJSON optionsIssueDescription

optionsIssueDescription :: Options
optionsIssueDescription =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("issueDescriptionLongDesc", "long_desc"),
        ("issueDescriptionShortDesc", "short_desc")
      ]

-- | Describes time in its various forms. It can be a single point in time; duration; or a structured timetable of operations
data IssueExpectedResolutionTime = IssueExpectedResolutionTime
  { -- | Describes duration as per ISO8601 format
    issueExpectedResolutionTimeDuration :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssueExpectedResolutionTime where
  parseJSON = genericParseJSON optionsIssueExpectedResolutionTime

instance ToJSON IssueExpectedResolutionTime where
  toJSON = genericToJSON optionsIssueExpectedResolutionTime

optionsIssueExpectedResolutionTime :: Options
optionsIssueExpectedResolutionTime =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("issueExpectedResolutionTimeDuration", "duration")
      ]

-- |
data IssuePost200Response = IssuePost200Response
  { -- |
    issuePost200ResponseError :: Maybe IssuePost200ResponseError,
    -- |
    issuePost200ResponseMessage :: IssuePost200ResponseMessage
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
      [ ("issuePost200ResponseError", "error"),
        ("issuePost200ResponseMessage", "message")
      ]

-- | Describes an error object
data IssuePost200ResponseError = IssuePost200ResponseError
  { -- | Beckn specific error code. For full list of error codes, refer to error_codes.md in the root folder of this repo
    issuePost200ResponseErrorCode :: Text,
    -- | Human readable message describing the error
    issuePost200ResponseErrorMessage :: Maybe Text,
    -- | Path to json schema generating the error. Used only during json schema validation errors
    issuePost200ResponseErrorPath :: Maybe Text,
    -- |
    issuePost200ResponseErrorType :: Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssuePost200ResponseError where
  parseJSON = genericParseJSON optionsIssuePost200ResponseError

instance ToJSON IssuePost200ResponseError where
  toJSON = genericToJSON optionsIssuePost200ResponseError

optionsIssuePost200ResponseError :: Options
optionsIssuePost200ResponseError =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("issuePost200ResponseErrorCode", "code"),
        ("issuePost200ResponseErrorMessage", "message"),
        ("issuePost200ResponseErrorPath", "path"),
        ("issuePost200ResponseErrorType", "type")
      ]

-- |
data IssuePost200ResponseMessage = IssuePost200ResponseMessage
  { -- |
    issuePost200ResponseMessageAck :: IssuePost200ResponseMessageAck
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

-- | Describes the ACK response
data IssuePost200ResponseMessageAck = IssuePost200ResponseMessageAck
  { -- | Describe the status of the ACK response. If schema validation passes, status is ACK else it is NACK
    issuePost200ResponseMessageAckStatus :: Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssuePost200ResponseMessageAck where
  parseJSON = genericParseJSON optionsIssuePost200ResponseMessageAck

instance ToJSON IssuePost200ResponseMessageAck where
  toJSON = genericToJSON optionsIssuePost200ResponseMessageAck

optionsIssuePost200ResponseMessageAck :: Options
optionsIssuePost200ResponseMessageAck =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("issuePost200ResponseMessageAckStatus", "status")
      ]

-- |
data IssueReq = IssueReq
  { -- |
    issueReqContext :: IssueReqContext,
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

-- | Describes a beckn message context
data IssueReqContext = IssueReqContext
  { -- | Defines the Beckn API call. Any actions other than the enumerated actions are not supported by Beckn Protocol
    issueReqContextAction :: Text,
    -- | Unique id of the BAP. By default it is the fully qualified domain name of the BAP
    issueReqContextBapId :: Text,
    -- | URI of the BAP for accepting callbacks. Must have the same domain name as the bap_id
    issueReqContextBapUri :: Text,
    -- | Unique id of the BPP. By default it is the fully qualified domain name of the BPP
    issueReqContextBppId :: Maybe Text,
    -- | URI of the BPP. Must have the same domain name as the bap_id
    issueReqContextBppUri :: Maybe Text,
    -- | City code
    issueReqContextCity :: Text,
    -- | Version of Beckn core API specification being used
    issueReqContextCoreVersion :: Text,
    -- | Country code as per ISO 3166-1 and ISO 3166-2 format
    issueReqContextCountry :: Text,
    -- | Describes the domain of an object
    issueReqContextDomain :: Text,
    -- | The encryption public key of the sender
    issueReqContextKey :: Maybe Text,
    -- | This is a unique value which persists during a request / callback cycle
    issueReqContextMessageId :: Text,
    -- | Time of request generation in RFC3339 format
    issueReqContextTimestamp :: UTCTime,
    -- | This is a unique value which persists across all API calls from search through confirm
    issueReqContextTransactionId :: Text,
    -- | The duration in ISO8601 format after timestamp for which this message holds valid
    issueReqContextTtl :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssueReqContext where
  parseJSON = genericParseJSON optionsIssueReqContext

instance ToJSON IssueReqContext where
  toJSON = genericToJSON optionsIssueReqContext

optionsIssueReqContext :: Options
optionsIssueReqContext =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("issueReqContextAction", "action"),
        ("issueReqContextBapId", "bap_id"),
        ("issueReqContextBapUri", "bap_uri"),
        ("issueReqContextBppId", "bpp_id"),
        ("issueReqContextBppUri", "bpp_uri"),
        ("issueReqContextCity", "city"),
        ("issueReqContextCoreVersion", "core_version"),
        ("issueReqContextCountry", "country"),
        ("issueReqContextDomain", "domain"),
        ("issueReqContextKey", "key"),
        ("issueReqContextMessageId", "message_id"),
        ("issueReqContextTimestamp", "timestamp"),
        ("issueReqContextTransactionId", "transaction_id"),
        ("issueReqContextTtl", "ttl")
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

-- | Describes the issue resolution for the issue raised by a Complainant
data IssueResolution = IssueResolution
  { -- | Indicactes the action taken while addressing the issue.  ### Actions   1. resolve : indicating the issue being resolved.    2. reject : indicating that the issue is rejected by the respondent or the resolution provider.
    issueResolutionAction :: Maybe Text,
    -- |
    issueResolutionActionTriggered :: Text,
    -- | resolution remarks provided by the gro
    issueResolutionGroRemarks :: Maybe Text,
    -- | remarks of the resolution of the issue by the respondent
    issueResolutionLongDesc :: Maybe Text,
    -- | resolution
    issueResolutionShortDesc :: Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssueResolution where
  parseJSON = genericParseJSON optionsIssueResolution

instance ToJSON IssueResolution where
  toJSON = genericToJSON optionsIssueResolution

optionsIssueResolution :: Options
optionsIssueResolution =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("issueResolutionAction", "action"),
        ("issueResolutionActionTriggered", "action_triggered"),
        ("issueResolutionGroRemarks", "gro_remarks"),
        ("issueResolutionLongDesc", "long_desc"),
        ("issueResolutionShortDesc", "short_desc")
      ]

-- | source of the issue
data IssueSource = IssueSource
  { -- | unique identifier of the network participant of the source of this issue. It can be one of buyer-id in the buyer- interfacing app , or any of the relevant network provider id based on what causes the issue
    issueSourceNetworkParticipantId :: Maybe Text,
    -- | Indicates the source from where the issue is raised.  ### TYPES     1. Consumer : indicating that the Consumer(Buyer) of a product or a service has raised the issue     2. Seller : indicating that the Seller of a product or a service has raised the issue     3. Intefacing NP : indicating that the Interfacing NP has identified and raised the issue
    issueSourceType :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssueSource where
  parseJSON = genericParseJSON optionsIssueSource

instance ToJSON IssueSource where
  toJSON = genericToJSON optionsIssueSource

optionsIssueSource :: Options
optionsIssueSource =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("issueSourceNetworkParticipantId", "network_participant_id"),
        ("issueSourceType", "type")
      ]

-- |
data IssueStatusReq = IssueStatusReq
  { -- |
    issueStatusReqContext :: IssueReqContext,
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
    issueStatusReqMessageIssueId :: Text
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
      [ ("issueStatusReqMessageIssueId", "issue_id")
      ]

-- | Identifies what is the sub-category of this issue
data IssueSubCategory = IssueSubCategory
  {
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON IssueSubCategory where
  parseJSON = genericParseJSON optionsIssueSubCategory

instance ToJSON IssueSubCategory where
  toJSON = genericToJSON optionsIssueSubCategory

optionsIssueSubCategory :: Options
optionsIssueSubCategory =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      []

-- |
data Item = Item
  { -- |
    itemId :: Maybe Text,
    -- |
    itemQuantity :: Maybe Double
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Item where
  parseJSON = genericParseJSON optionsItem

instance ToJSON Item where
  toJSON = genericToJSON optionsItem

optionsItem :: Options
optionsItem =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("itemId", "id"),
        ("itemQuantity", "quantity")
      ]

-- |
data OnIssueReq = OnIssueReq
  { -- |
    onIssueReqContext :: IssueReqContext,
    -- |
    onIssueReqError :: Maybe IssuePost200ResponseError,
    -- |
    onIssueReqMessage :: OnIssueReqMessage
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
        ("onIssueReqError", "error"),
        ("onIssueReqMessage", "message")
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
    onIssueStatusReqContext :: IssueReqContext,
    -- |
    onIssueStatusReqError :: Maybe IssuePost200ResponseError,
    -- |
    onIssueStatusReqMessage :: IssueReqMessage
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
        ("onIssueStatusReqError", "error"),
        ("onIssueStatusReqMessage", "message")
      ]

-- |
data OrderDetails = OrderDetails
  { -- |
    orderDetailsFulfillments :: Maybe [Fulfillment],
    -- | the value of this field will be the combination of context.transaction_id and order.id.
    orderDetailsId :: Maybe Text,
    -- |
    orderDetailsItems :: Maybe [Item],
    -- |
    orderDetailsProviderId :: Maybe Text,
    -- |
    orderDetailsState :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OrderDetails where
  parseJSON = genericParseJSON optionsOrderDetails

instance ToJSON OrderDetails where
  toJSON = genericToJSON optionsOrderDetails

optionsOrderDetails :: Options
optionsOrderDetails =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("orderDetailsFulfillments", "fulfillments"),
        ("orderDetailsId", "id"),
        ("orderDetailsItems", "items"),
        ("orderDetailsProviderId", "provider_id"),
        ("orderDetailsState", "state")
      ]

-- |
data Organization = Organization
  { -- |
    organizationContact :: Maybe GROContact,
    -- |
    organizationOrg :: Maybe OrganizationOrg,
    -- |
    organizationPerson :: Maybe ComplainantPerson
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Organization where
  parseJSON = genericParseJSON optionsOrganization

instance ToJSON Organization where
  toJSON = genericToJSON optionsOrganization

optionsOrganization :: Options
optionsOrganization =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("organizationContact", "contact"),
        ("organizationOrg", "org"),
        ("organizationPerson", "person")
      ]

-- | Describes an organization
data OrganizationOrg = OrganizationOrg
  { -- |
    organizationOrgName :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OrganizationOrg where
  parseJSON = genericParseJSON optionsOrganizationOrg

instance ToJSON OrganizationOrg where
  toJSON = genericToJSON optionsOrganizationOrg

optionsOrganizationOrg :: Options
optionsOrganizationOrg =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("organizationOrgName", "name")
      ]

-- | Describes the details of the entity that provides the resolution for the issue
data ResolutionProvider = ResolutionProvider
  { -- |
    resolutionProviderRespondentInfo :: ResolutionProviderRespondentInfo
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ResolutionProvider where
  parseJSON = genericParseJSON optionsResolutionProvider

instance ToJSON ResolutionProvider where
  toJSON = genericToJSON optionsResolutionProvider

optionsResolutionProvider :: Options
optionsResolutionProvider =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("resolutionProviderRespondentInfo", "respondent_info")
      ]

-- | the respondent represents the details of the entity identfied by the resolution provider or the complainant himself (when he has to select the ODRs)
data ResolutionProviderRespondentInfo = ResolutionProviderRespondentInfo
  { -- |
    resolutionProviderRespondentInfoOrganization :: Maybe Organization,
    -- |
    resolutionProviderRespondentInfoResolutionSupport :: Maybe ResolutionSupport,
    -- | Indicates the type of the resolution provider NP type. ### TYPES 1. Interfacing NP : indicating the NP where or through which the issue is raised 2. Transaction Counterparty NP : indicating the first level of transaction counterparty network provider with which the interfacing NP is interacting with to get an issue resolution. 3. Cascaded Counterparty NP : indicating the second or any further downstream levels of counterparty network provider with which the NP is interacting with to get an issue resolution.
    resolutionProviderRespondentInfoType :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ResolutionProviderRespondentInfo where
  parseJSON = genericParseJSON optionsResolutionProviderRespondentInfo

instance ToJSON ResolutionProviderRespondentInfo where
  toJSON = genericToJSON optionsResolutionProviderRespondentInfo

optionsResolutionProviderRespondentInfo :: Options
optionsResolutionProviderRespondentInfo =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("resolutionProviderRespondentInfoOrganization", "organization"),
        ("resolutionProviderRespondentInfoResolutionSupport", "resolution_support"),
        ("resolutionProviderRespondentInfoType", "type")
      ]

-- | Any one of these resolution support deatils are mandatory to be speficed whenever an issue resolution is proposed. In case of Issue, Either a respondent ChatLink, email or faq deatils should be provided or any of the additional sources of support should be provided In case of Grievance : the GRO details are manadatory to be provided  In case of Dispute : the selected ODR details are mandatory to be provided  Porvides details of the resolution support for the respondent / complainant to interact with the respondent
data ResolutionSupport = ResolutionSupport
  { -- |
    resolutionSupportContact :: Maybe GROContact,
    -- | Details of the GRO appointed by the resolution provider to support in issue resolution . This is a required object if and when the issue type is \"Grievance\"
    resolutionSupportGros :: Maybe [GRO]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ResolutionSupport where
  parseJSON = genericParseJSON optionsResolutionSupport

instance ToJSON ResolutionSupport where
  toJSON = genericToJSON optionsResolutionSupport

optionsResolutionSupport :: Options
optionsResolutionSupport =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("resolutionSupportContact", "contact"),
        ("resolutionSupportGros", "gros")
      ]

-- |
data RespondentAction = RespondentAction
  { -- |
    respondentActionRespondentAction :: Maybe Text,
    -- | details of the remarks when the issue status is changed, can be captured when the respondent triggers an action
    respondentActionShortDesc :: Maybe Text,
    -- | timestamp for the capturing the time an issue status was last updated
    respondentActionUpdatedAt :: Maybe UTCTime,
    -- |
    respondentActionUpdatedBy :: Maybe Organization
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RespondentAction where
  parseJSON = genericParseJSON optionsRespondentAction

instance ToJSON RespondentAction where
  toJSON = genericToJSON optionsRespondentAction

optionsRespondentAction :: Options
optionsRespondentAction =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("respondentActionRespondentAction", "respondent_action"),
        ("respondentActionShortDesc", "short_desc"),
        ("respondentActionUpdatedAt", "updated_at"),
        ("respondentActionUpdatedBy", "updated_by")
      ]

-- | Describes a tag. This is used to contain extended metadata. This object can be added as a property to any schema to describe extended attributes. For BAPs, tags can be sent during search to optimize and filter search results. BPPs can use tags to index their catalog to allow better search functionality. Tags are sent by the BPP as part of the catalog response in the &#x60;on_search&#x60; callback. Tags are also meant for display purposes. Upon receiving a tag, BAPs are meant to render them as name-value pairs. This is particularly useful when rendering tabular information about a product or service.
data Tag = Tag
  { -- |
    tagDescriptor :: Maybe Descriptor,
    -- | This value indicates if the tag is intended for display purposes. If set to `true`, then this tag must be displayed. If it is set to `false`, it should not be displayed. This value can override the group display value.
    tagDisplay :: Maybe Bool,
    -- | The value of the tag. This set by the BPP and rendered as-is by the BAP.
    tagValue :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data, Read)

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
        ("tagDisplay", "display"),
        ("tagValue", "value")
      ]

-- | A collection of tag objects with group level attributes. For detailed documentation on the Tags and Tag Groups schema go to https://github.com/beckn/protocol-specifications/discussions/316
data TagGroup = TagGroup
  { -- |
    tagGroupDescriptor :: Maybe Descriptor,
    -- | Indicates the display properties of the tag group. If display is set to false, then the group will not be displayed. If it is set to true, it should be displayed. However, group-level display properties can be overriden by individual tag-level display property. As this schema is purely for catalog display purposes, it is not recommended to send this value during search.
    tagGroupDisplay :: Maybe Bool,
    -- | An array of Tag objects listed under this group. This property can be set by BAPs during search to narrow the `search` and achieve more relevant results. When received during `on_search`, BAPs must render this list under the heading described by the `name` property of this schema.
    tagGroupList :: Maybe [Tag]
  }
  deriving (Show, Eq, Generic, Data, Read)

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
      [ ("tagGroupDescriptor", "descriptor"),
        ("tagGroupDisplay", "display"),
        ("tagGroupList", "list")
      ]
