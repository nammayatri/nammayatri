{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IGM.Enums where

import Data.Aeson
import Data.Aeson.Types (parseFail, typeMismatch)
import Data.Set (Set)
import qualified Data.Set as Set
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.GenericPretty
import Kernel.Utils.JSON
import Prelude (show)

data Domain
  = ON_DEMAND
  | PUBLIC_TRANSPORT
  deriving (Eq, Generic, Read, FromDhall, ToSchema, Ord)
  deriving (PrettyShow) via Showable Domain

instance Show Domain where
  show ON_DEMAND = "ONDC:TRV10"
  show PUBLIC_TRANSPORT = "ONDC:TRV11"

instance FromJSON Domain where
  parseJSON (String "ONDC:TRV10") = pure ON_DEMAND
  parseJSON (String "ONDC:TRV11") = pure PUBLIC_TRANSPORT
  parseJSON (String _) = parseFail "Invalid Domain"
  parseJSON e = typeMismatch "String" e

instance ToJSON Domain where
  toJSON ON_DEMAND = String "ONDC:TRV10"
  toJSON PUBLIC_TRANSPORT = String "ONDC:TRV11"

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
    ORDER
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

data RespondentActions
  = -- ..issue.issue_action.respondent_actions.respondent_action
    PROCESSING
  | RESOLVED
  | CASCADED
  | NEED_MORE_INFO
  deriving (Eq, Generic)

instance Show RespondentActions where
  show PROCESSING = "PROCESSING"
  show RESOLVED = "RESOLVED"
  show CASCADED = "CASCADED"
  show NEED_MORE_INFO = "NEED-MORE-INFO"

instance ToJSON RespondentActions where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON RespondentActions where
  parseJSON (String "PROCESSING") = return PROCESSING
  parseJSON (String "RESOLVED") = return RESOLVED
  parseJSON (String "CASCADED") = return CASCADED
  parseJSON (String "NEED-MORE-INFO") = return NEED_MORE_INFO
  parseJSON wrongVal = typeMismatch "Invalid RespondentActions" wrongVal

data ResolutionAction
  = -- ..issues.resolution.action
    RESOLVE
  | REJECT
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ResolutionActionTriggered
  = -- ..issues.resolution.action_triggered
    REFUND
  | CANCEL
  | NO_ACTION
  deriving (Eq, Generic)

instance Show ResolutionActionTriggered where
  show REFUND = "REFUND"
  show CANCEL = "CANCEL"
  show NO_ACTION = "NO-ACTION"

instance ToJSON ResolutionActionTriggered where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON ResolutionActionTriggered where
  parseJSON (String "REFUND") = return REFUND
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

data RespondentInfo
  = -- ..issue.resolution_provider.respondent_info
    RESPONDING_INTERFACING_NP
  | TRANSACTION_COUNTERPARTY_NP
  | CASCADED_COUNTERPARTY_NP
  deriving (Eq, Generic)

instance Show RespondentInfo where
  show RESPONDING_INTERFACING_NP = "INTERFACING-NP"
  show TRANSACTION_COUNTERPARTY_NP = "TRANSACTION-COUNTERPARTY-NP"
  show CASCADED_COUNTERPARTY_NP = "CASCADED-COUNTERPARTY-NP"

instance ToJSON RespondentInfo where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON RespondentInfo where
  parseJSON (String "INTERFACING-NP") = return RESPONDING_INTERFACING_NP
  parseJSON (String "TRANSACTION-COUNTERPARTY-NP") = return TRANSACTION_COUNTERPARTY_NP
  parseJSON (String "CASCADED-COUNTERPARTY-NP") = return CASCADED_COUNTERPARTY_NP
  parseJSON wrongVal = typeMismatch "Invalid Respon```` ```````dentInfo" wrongVal

data IssueSubCategory
  = -- ..issue.sub_category
    FLM111 -- "Driver unable to end trip"
  | FLM112 -- "Driver took a circuitious route/longer route  /  Driver took a different route"
  | FLM113 -- "Trip OTP isn't available in the app"
  | FLM114 -- "Trip OTP not working when paired with the Driver app"
  | FLM115 -- "Vehicle brokedown while on trip"
  | ORD111 -- "Report lost item to support"
  | PMT111 -- "Driver asked for extra fare / demanded more fare"
  | PMT112 -- "Mismatch between fare shown at the beginning and at the end of the trip"
  | PMT113 -- "Amout paid extra by mistake"
  | PMT114 -- "Driver asked me to pay for airport pickup charges/railway station pickup charges"
  | PMT115 -- "Customer refused to pay for parking fee during rental ride"
  | PMT116 -- "Customer refused to pay revised fare (for extra km and time)"
  | PMT117 -- "Driver asked to pay for toll separately while it was already computed in the initial fare"
  | PMT118 -- "Driver refused to take the toll road despite the toll fee already being paid"
  | PMT119 -- "Delayed payment by collector entity and concerns on delayed interest"
  | ORD101 -- "Ticket not generated post payment completion"
  | FLM101 -- "Generated ticket not working at the ticketing gate/validation error"
  | FLM102 -- "Cancellation of the generated ticket not working"
  | FLM103 -- "Change of entry/exit station details"
  | FLM104 -- "Origin & Destination mismatch"
  | PMT101 -- "Refund to buyer not processed for cancellation (/partial) & payment failure scenarios"
  | PMT102 -- "Amount deducted from Buyer but payment failed status"
  | PMT103 -- "Collector entity overpays receiver entity"
  | PMT104 -- "Settlement to receiver account not done as per settlement period"
  | PMT105 -- "Partial payment done to receiver entity"
  | PMT106 -- "delayed payment by collector entity and concerns on delayed interest"
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnumAndList ''IssueSubCategory)

metroSubcategories :: Set IssueSubCategory
metroSubcategories =
  Set.fromList
    [ ORD101,
      FLM101,
      FLM102,
      FLM103,
      FLM104,
      PMT101,
      PMT102,
      PMT103,
      PMT104,
      PMT105,
      PMT106
    ]

onDemandSubcategories :: Set IssueSubCategory
onDemandSubcategories =
  Set.fromList
    [ FLM111,
      FLM112,
      FLM113,
      FLM114,
      FLM115,
      ORD111,
      PMT111,
      PMT112,
      PMT113,
      PMT114,
      PMT115,
      PMT116,
      PMT117,
      PMT118,
      PMT119
    ]
