{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module IGM.Enums where

import Data.Aeson
import Data.Aeson.Types (parseFail, typeMismatch)
import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.GenericPretty
import Kernel.Utils.JSON
import Prelude (show)

data Domain
  = ON_DEMAND
  deriving (Eq, Generic, Show, Read, FromDhall, ToSchema)
  deriving (PrettyShow) via Showable Domain

instance FromJSON Domain where
  parseJSON (String "ONDC:TRV10") = pure ON_DEMAND
  parseJSON (String _) = parseFail "Invalid Domain"
  parseJSON e = typeMismatch "String" e

instance ToJSON Domain where
  toJSON ON_DEMAND = String "ONDC:TRV10"

data Action
  = ISSUE
  | ISSUE_STATUS
  | ON_ISSUE
  | ON_ISSUE_STATUS
  deriving (Generic, Show, Eq, ToSchema)
  deriving (PrettyShow) via Showable Action

instance FromJSON Action where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON Action where
  toJSON = genericToJSON constructorsToLowerOptions

data IssueType
  = -- ..issue.issue_type
    TYPE_ISSUE
  | GRIEVANCE
  | DISPUTE
  deriving (Eq, Generic)

instance Show IssueType where
  show TYPE_ISSUE = "ISSUE"
  show GRIEVANCE = "GRIEVANCE"
  show DISPUTE = "DISPUTE"

instance ToJSON IssueType where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON IssueType where
  parseJSON (String "ISSUE") = return TYPE_ISSUE
  parseJSON (String "GRIEVANCE") = return GRIEVANCE
  parseJSON (String "DISPUTE") = return DISPUTE
  parseJSON wrongVal = typeMismatch "Invalid IssueType" wrongVal

-- data OrderDetailsState
--   = -- ..issue.order_details.state
--     COMPLETED
--   deriving (Show, Eq, Generic, ToJSON, FromJSON)

data IssueCategory
  = -- ..issue.category
    TRANSACTION
  | ORDER
  | ITEM
  | FULFILLMENT
  | AGENT
  | PAYMENT
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data IssueSourceType
  = -- ..issue.source.type INTERFACING-NP
    CONSUMER
  | SELLER
  | INTERFACING_NP
  deriving (Eq, Generic)

instance Show IssueSourceType where
  show CONSUMER = "CONSUMER"
  show SELLER = "SELLER"
  show INTERFACING_NP = "INTERFACING-NP"

instance ToJSON IssueSourceType where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON IssueSourceType where
  parseJSON (String "CONSUMER") = return CONSUMER
  parseJSON (String "SELLER") = return SELLER
  parseJSON (String "INTERFACING-NP") = return INTERFACING_NP
  parseJSON wrongVal = typeMismatch "Invalid IssueSourceType" wrongVal

data IssueStatus
  = -- ..issue.status
    OPEN
  | CLOSED
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ComplaintAction
  = -- ..issue.issue_action.complainant_actions.complainant_action
    OPEN_ISSUE
  | ESCALATE
  | CLOSE
  deriving (Eq, Generic)

instance Show ComplaintAction where
  show OPEN_ISSUE = "OPEN"
  show ESCALATE = "ESCALATE"
  show CLOSE = "CLOSE"

instance ToJSON ComplaintAction where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON ComplaintAction where
  parseJSON (String "OPEN") = return OPEN_ISSUE
  parseJSON (String "ESCALATE") = return ESCALATE
  parseJSON (String "CLOSE") = return CLOSE
  parseJSON wrongVal = typeMismatch "Invalid ComplaintAction" wrongVal

data IssueRating
  = -- ..issue.issue_rating
    THUMBS_UP
  | THUMBS_DOWN
  deriving (Eq, Generic)

instance Show IssueRating where
  show THUMBS_UP = "THUMBS-UP"
  show THUMBS_DOWN = "THUMBS-DOWN"

instance ToJSON IssueRating where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON IssueRating where
  parseJSON (String "THUMBS-UP") = return THUMBS_UP
  parseJSON (String "THUMBS-DOWN") = return THUMBS_DOWN
  parseJSON wrongVal = typeMismatch "Invalid IssueRating" wrongVal

data RespondantAction
  = -- ..issue.issue_action.respondant_actions.respondant_action
    PROCESSING
  | RESOLVED
  | CASCADED
  | NEED_MORE_INFO
  deriving (Eq, Generic)

instance Show RespondantAction where
  show PROCESSING = "PROCESSING"
  show RESOLVED = "RESOLVED"
  show CASCADED = "CASCADED"
  show NEED_MORE_INFO = "NEED-MORE-INFO"

instance ToJSON RespondantAction where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON RespondantAction where
  parseJSON (String "PROCESSING") = return PROCESSING
  parseJSON (String "RESOLVED") = return RESOLVED
  parseJSON (String "CASCADED") = return CASCADED
  parseJSON (String "NEED-MORE-INFO") = return NEED_MORE_INFO
  parseJSON wrongVal = typeMismatch "Invalid RespondantAction" wrongVal

data ResolutionAction
  = -- ..issues.resolution.action
    RESOLVE
  | REJECT
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ResolutionActionTriggered
  = -- ..issues.resolution.action_triggered
    REFUND
  | REPLACEMENT
  | RETURN
  | CANCEL
  | NO_ACTION
  deriving (Eq, Generic)

instance Show ResolutionActionTriggered where
  show REFUND = "REFUND"
  show REPLACEMENT = "REPLACEMENT"
  show RETURN = "RETURN"
  show CANCEL = "CANCEL"
  show NO_ACTION = "NO-ACTION"

instance ToJSON ResolutionActionTriggered where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON ResolutionActionTriggered where
  parseJSON (String "REFUND") = return REFUND
  parseJSON (String "REPLACEMENT") = return REPLACEMENT
  parseJSON (String "CANCEL") = return CANCEL
  parseJSON (String "NO-ACTION") = return NO_ACTION
  parseJSON wrongVal = typeMismatch "Invalid ResolutionActionTriggered" wrongVal

data ErrorType
  = -- response error.type
    CONTEXT_ERROR
  | CORE_ERROR
  | DOMAIN_ERROR
  | POLICY_ERROR
  | JSON_SCHEMA_ERROR
  deriving (Eq, Generic)

instance Show ErrorType where
  show CONTEXT_ERROR = "CONTEXT-ERROR"
  show CORE_ERROR = "CORE-ERROR"
  show DOMAIN_ERROR = "DOMAIN-ERROR"
  show POLICY_ERROR = "POLICY-ERROR"
  show JSON_SCHEMA_ERROR = "JSON-SCHEMA-ERROR"

instance ToJSON ErrorType where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON ErrorType where
  parseJSON (String "CONTEXT-ERROR") = return CONTEXT_ERROR
  parseJSON (String "CORE-ERROR") = return CORE_ERROR
  parseJSON (String "DOMAIN-ERROR") = return DOMAIN_ERROR
  parseJSON (String "POLICY-ERROR") = return POLICY_ERROR
  parseJSON (String "JSON-SCHEMA-ERROR") = return JSON_SCHEMA_ERROR
  parseJSON wrongVal = typeMismatch "Invalid ErrorType" wrongVal

data RespondantInfo
  = -- ..issue.resolution_provider.respondant_info
    RESPONDING_INTERFACING_NP
  | TRANSACTION_COUNTERPARTY_NP
  | CASCADED_COUNTERPARTY_NP
  deriving (Eq, Generic)

instance Show RespondantInfo where
  show RESPONDING_INTERFACING_NP = "INTERFACING-NP"
  show TRANSACTION_COUNTERPARTY_NP = "TRANSACTION-COUNTERPARTY-NP"
  show CASCADED_COUNTERPARTY_NP = "CASCADED-COUNTERPARTY-NP"

instance ToJSON RespondantInfo where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON RespondantInfo where
  parseJSON (String "INTERFACING-NP") = return RESPONDING_INTERFACING_NP
  parseJSON (String "TRANSACTION-COUNTERPARTY-NP") = return TRANSACTION_COUNTERPARTY_NP
  parseJSON (String "CASCADED-COUNTERPARTY-NP") = return CASCADED_COUNTERPARTY_NP
  parseJSON wrongVal = typeMismatch "Invalid RespondantInfo" wrongVal
