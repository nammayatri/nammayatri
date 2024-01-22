{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module BecknV2.OnDemand.Types
  ( Ack (..),
    AckMessage (..),
    AckResponse (..),
    Agent (..),
    Authorization (..),
    Billing (..),
    CancelReq (..),
    CancelReqMessage (..),
    Cancellation (..),
    CancellationTerm (..),
    Catalog (..),
    City (..),
    ConfirmReq (..),
    ConfirmReqMessage (..),
    Contact (..),
    Context (..),
    Country (..),
    Customer (..),
    Descriptor (..),
    Domain (..),
    Error (..),
    Fee (..),
    FeedbackForm (..),
    Fulfillment (..),
    FulfillmentState (..),
    Image (..),
    InitReq (..),
    Intent (..),
    Item (..),
    Location (..),
    OnConfirmReq (..),
    OnInitReq (..),
    OnSearchReq (..),
    OnSearchReqMessage (..),
    OnSelectReq (..),
    OnSelectReqMessage (..),
    OnStatusReq (..),
    OnTrackReq (..),
    OnTrackReqMessage (..),
    OnUpdateReq (..),
    Option (..),
    Order (..),
    Payment (..),
    PaymentParams (..),
    Person (..),
    Price (..),
    Provider (..),
    Quotation (..),
    QuotationBreakupInner (..),
    Rating (..),
    RatingReq (..),
    RatingReqMessage (..),
    SearchReq (..),
    SearchReqMessage (..),
    SelectReq (..),
    State (..),
    StatusReq (..),
    StatusReqMessage (..),
    Stop (..),
    Tag (..),
    TagGroup (..),
    Time (..),
    TrackReq (..),
    TrackReqMessage (..),
    Tracking (..),
    Vehicle (..),
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
      [ ("ackStatus", "status"),
        ("ackTags", "tags")
      ]

-- |
newtype AckMessage = AckMessage
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

-- | Describes the direct performer, driver or executor that fulfills an order. It is usually a person. But in some rare cases, it could be a non-living entity like a drone, or a bot. Some examples of agents are Doctor in the healthcare sector, a driver in the mobility sector, or a delivery person in the logistics sector. This object can be set at any stage of the order lifecycle. This can be set at the discovery stage when the BPP wants to provide details on the agent fulfilling the order, like in healthcare, where the doctor&#39;s name appears during search. This object can also used to search for a particular person that the customer wants fulfilling an order. Sometimes, this object gets instantiated after the order is confirmed, like in the case of on-demand taxis, where the driver is assigned after the user confirms the ride.
data Agent = Agent
  { -- |
    agentContact :: Maybe Contact,
    -- |
    agentPerson :: Maybe Person
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Agent where
  parseJSON = genericParseJSON optionsAgent

instance ToJSON Agent where
  toJSON = genericToJSON optionsAgent

optionsAgent :: Options
optionsAgent =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("agentContact", "contact"),
        ("agentPerson", "person")
      ]

-- | Describes an authorization mechanism used to start or end the fulfillment of an order. For example, in the mobility sector, the driver may require a one-time password to initiate the ride. In the healthcare sector, a patient may need to provide a password to open a video conference link during a teleconsultation.
data Authorization = Authorization
  { -- | Token used for authorization. This is typically generated at the BPP. The BAP can send this value to the user via any channel that it uses to authenticate the user like SMS, Email, Push notification, or in-app rendering.
    authorizationToken :: Maybe Text,
    -- | Type of authorization mechanism used. The allowed values for this field can be published as part of the network policy.
    authorizationType :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Authorization where
  parseJSON = genericParseJSON optionsAuthorization

instance ToJSON Authorization where
  toJSON = genericToJSON optionsAuthorization

optionsAuthorization :: Options
optionsAuthorization =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("authorizationToken", "token"),
        ("authorizationType", "type")
      ]

-- | Describes the billing details of an entity.&lt;br&gt;This has properties like name,organization,address,email,phone,time,tax_number, created_at,updated_at
newtype Billing = Billing
  { -- | Phone number of the billable entity
    billingPhone :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Billing where
  parseJSON = genericParseJSON optionsBilling

instance ToJSON Billing where
  toJSON = genericToJSON optionsBilling

optionsBilling :: Options
optionsBilling =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("billingPhone", "phone")
      ]

-- |
data CancelReq = CancelReq
  { -- |
    cancelReqContext :: Context,
    -- |
    cancelReqMessage :: CancelReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON CancelReq where
  parseJSON = genericParseJSON optionsCancelReq

instance ToJSON CancelReq where
  toJSON = genericToJSON optionsCancelReq

optionsCancelReq :: Options
optionsCancelReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cancelReqContext", "context"),
        ("cancelReqMessage", "message")
      ]

-- |
-- |
data CancelReqMessage = CancelReqMessage
  { -- |
    cancelReqMessageCancellationReasonId :: Maybe Text,
    -- |
    cancelReqMessageDescriptor :: Maybe Descriptor,
    -- |
    cancelReqMessageOrderId :: Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON CancelReqMessage where
  parseJSON = genericParseJSON optionsCancelReqMessage

instance ToJSON CancelReqMessage where
  toJSON = genericToJSON optionsCancelReqMessage

optionsCancelReqMessage :: Options
optionsCancelReqMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cancelReqMessageCancellationReasonId", "cancellation_reason_id"),
        ("cancelReqMessageDescriptor", "descriptor"),
        ("cancelReqMessageOrderId", "order_id")
      ]

-- | Describes a cancellation event
newtype Cancellation = Cancellation
  { -- |
    cancellationCancelledBy :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Cancellation where
  parseJSON = genericParseJSON optionsCancellation

instance ToJSON Cancellation where
  toJSON = genericToJSON optionsCancellation

optionsCancellation :: Options
optionsCancellation =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cancellationCancelledBy", "cancelled_by")
      ]

-- | Describes the cancellation terms of an item or an order. This can be referenced at an item or order level. Item-level cancellation terms can override the terms at the order level.
data CancellationTerm = CancellationTerm
  { -- |
    cancellationTermCancellationFee :: Maybe Fee,
    -- |
    cancellationTermFulfillmentState :: Maybe FulfillmentState,
    -- | Indicates whether a reason is required to cancel the order
    cancellationTermReasonRequired :: Maybe Bool
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON CancellationTerm where
  parseJSON = genericParseJSON optionsCancellationTerm

instance ToJSON CancellationTerm where
  toJSON = genericToJSON optionsCancellationTerm

optionsCancellationTerm :: Options
optionsCancellationTerm =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cancellationTermCancellationFee", "cancellation_fee"),
        ("cancellationTermFulfillmentState", "fulfillment_state"),
        ("cancellationTermReasonRequired", "reason_required")
      ]

-- | Describes the products or services offered by a BPP. This is typically sent as the response to a search intent from a BAP. The payment terms, offers and terms of fulfillment supported by the BPP can also be included here. The BPP can show hierarchical nature of products/services in its catalog using the parent_category_id in categories. The BPP can also send a ttl (time to live) in the context which is the duration for which a BAP can cache the catalog and use the cached catalog.  &lt;br&gt;This has properties like bbp/descriptor,bbp/categories,bbp/fulfillments,bbp/payments,bbp/offers,bbp/providers and exp&lt;br&gt;This is used in the following situations.&lt;br&gt;&lt;ul&gt;&lt;li&gt;This is typically used in the discovery stage when the BPP sends the details of the products and services it offers as response to a search intent from the BAP. &lt;/li&gt;&lt;/ul&gt;
data Catalog = Catalog
  { -- |
    catalogDescriptor :: Maybe Descriptor,
    -- |
    catalogProviders :: Maybe [Provider]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Catalog where
  parseJSON = genericParseJSON optionsCatalog

instance ToJSON Catalog where
  toJSON = genericToJSON optionsCatalog

optionsCatalog :: Options
optionsCatalog =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("catalogDescriptor", "descriptor"),
        ("catalogProviders", "providers")
      ]

-- | Describes a city
data City = City
  { -- | City code
    cityCode :: Maybe Text,
    -- | Name of the city
    cityName :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON City where
  parseJSON = genericParseJSON optionsCity

instance ToJSON City where
  toJSON = genericToJSON optionsCity

optionsCity :: Options
optionsCity =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cityCode", "code"),
        ("cityName", "name")
      ]

-- |
data ConfirmReq = ConfirmReq
  { -- |
    confirmReqContext :: Context,
    -- |
    confirmReqMessage :: ConfirmReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ConfirmReq where
  parseJSON = genericParseJSON optionsConfirmReq

instance ToJSON ConfirmReq where
  toJSON = genericToJSON optionsConfirmReq

optionsConfirmReq :: Options
optionsConfirmReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("confirmReqContext", "context"),
        ("confirmReqMessage", "message")
      ]

-- |
-- |
newtype ConfirmReqMessage = ConfirmReqMessage
  { -- |
    confirmReqMessageOrder :: Order
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ConfirmReqMessage where
  parseJSON = genericParseJSON optionsConfirmReqMessage

instance ToJSON ConfirmReqMessage where
  toJSON = genericToJSON optionsConfirmReqMessage

optionsConfirmReqMessage :: Options
optionsConfirmReqMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("confirmReqMessageOrder", "order")
      ]

-- | Describes the contact information of an entity
newtype Contact = Contact
  { -- |
    contactPhone :: Maybe Text
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
      [ ("contactPhone", "phone")
      ]

-- | Every API call in beckn protocol has a context. It provides a high-level overview to the receiver about the nature of the intended transaction. Typically, it is the BAP that sets the transaction context based on the consumer&#39;s location and action on their UI. But sometimes, during unsolicited callbacks, the BPP also sets the transaction context but it is usually the same as the context of a previous full-cycle, request-callback interaction between the BAP and the BPP. The context object contains four types of fields. &lt;ol&gt;&lt;li&gt;Demographic information about the transaction using fields like &#x60;domain&#x60;, &#x60;country&#x60;, and &#x60;region&#x60;.&lt;/li&gt;&lt;li&gt;Addressing details like the sending and receiving platform&#39;s ID and API URL.&lt;/li&gt;&lt;li&gt;Interoperability information like the protocol version that implemented by the sender and,&lt;/li&gt;&lt;li&gt;Transaction details like the method being called at the receiver&#39;s endpoint, the transaction_id that represents an end-to-end user session at the BAP, a message ID to pair requests with callbacks, a timestamp to capture sending times, a ttl to specifiy the validity of the request, and a key to encrypt information if necessary.&lt;/li&gt;&lt;/ol&gt; This object must be passed in every interaction between a BAP and a BPP. In HTTP/S implementations, it is not necessary to send the context during the synchronous response. However, in asynchronous protocols, the context must be sent during all interactions,
data Context = Context
  { -- | The Beckn protocol method being called by the sender and executed at the receiver.
    contextAction :: Maybe Text,
    -- | A globally unique identifier of the platform, Typically it is the fully qualified domain name (FQDN) of the platform.
    contextBapId :: Maybe Text,
    -- | The callback URL of the Subscriber. This should necessarily contain the same domain name as set in `subscriber_id``.
    contextBapUri :: Maybe Text,
    -- |
    contextBppId :: Maybe Text,
    -- |
    contextBppUri :: Maybe Text,
    -- |
    contextDomain :: Maybe Text,
    -- | The encryption public key of the sender
    contextKey :: Maybe Text,
    -- |
    contextLocation :: Maybe Location,
    -- | This is a unique value which persists during a request / callback cycle. Since beckn protocol APIs are asynchronous, BAPs need a common value to match an incoming callback from a BPP to an earlier call. This value can also be used to ignore duplicate messages coming from the BPP. It is recommended to generate a fresh message_id for every new interaction. When sending unsolicited callbacks, BPPs must generate a new message_id.
    contextMessageId :: Maybe UUID,
    -- | Time of request generation in RFC3339 format
    contextTimestamp :: Maybe UTCTime,
    -- | This is a unique value which persists across all API calls from `search` through `confirm`. This is done to indicate an active user session across multiple requests. The BPPs can use this value to push personalized recommendations, and dynamic offerings related to an ongoing transaction despite being unaware of the user active on the BAP.
    contextTransactionId :: Maybe UUID,
    -- | The duration in ISO8601 format after timestamp for which this message holds valid
    contextTtl :: Maybe Text,
    -- | Version of transaction protocol being used by the sender.
    contextVersion :: Maybe Text
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
      [ ("contextAction", "action"),
        ("contextBapId", "bap_id"),
        ("contextBapUri", "bap_uri"),
        ("contextBppId", "bpp_id"),
        ("contextBppUri", "bpp_uri"),
        ("contextDomain", "domain"),
        ("contextKey", "key"),
        ("contextLocation", "location"),
        ("contextMessageId", "message_id"),
        ("contextTimestamp", "timestamp"),
        ("contextTransactionId", "transaction_id"),
        ("contextTtl", "ttl"),
        ("contextVersion", "version")
      ]

-- | Describes a country
data Country = Country
  { -- | Country code as per ISO 3166-1 and ISO 3166-2 format
    countryCode :: Maybe Text,
    -- | Name of the country
    countryName :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Country where
  parseJSON = genericParseJSON optionsCountry

instance ToJSON Country where
  toJSON = genericToJSON optionsCountry

optionsCountry :: Options
optionsCountry =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("countryCode", "code"),
        ("countryName", "name")
      ]

-- | Describes a customer buying/availing a product or a service
data Customer = Customer
  { -- |
    customerContact :: Maybe Contact,
    -- |
    customerPerson :: Maybe Person
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Customer where
  parseJSON = genericParseJSON optionsCustomer

instance ToJSON Customer where
  toJSON = genericToJSON optionsCustomer

optionsCustomer :: Options
optionsCustomer =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("customerContact", "contact"),
        ("customerPerson", "person")
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
      [ ("descriptorCode", "code"),
        ("descriptorName", "name"),
        ("descriptorShortDesc", "short_desc")
      ]

-- | Described the industry sector or sub-sector. The network policy should contain codes for all the industry sectors supported by the network. Domains can be created in varying levels of granularity. The granularity of a domain can be decided by the participants of the network. Too broad domains will result in irrelevant search broadcast calls to BPPs that don&#39;t have services supporting the domain. Too narrow domains will result in a large number of registry entries for each BPP. It is recommended that network facilitators actively collaborate with various working groups and network participants to carefully choose domain codes keeping in mind relevance, performance, and opportunity cost. It is recommended that networks choose broad domains like mobility, logistics, healthcare etc, and progressively granularize them as and when the number of network participants for each domain grows large.
newtype Domain = Domain
  { -- | Standard code representing the domain. The standard is usually published as part of the network policy. Furthermore, the network facilitator should also provide a mechanism to provide the supported domains of a network.
    domainCode :: Maybe Value
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Domain where
  parseJSON = genericParseJSON optionsDomain

instance ToJSON Domain where
  toJSON = genericToJSON optionsDomain

optionsDomain :: Options
optionsDomain =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("domainCode", "code")
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
      [ ("errorCode", "code"),
        ("errorMessage", "message"),
        ("errorPaths", "paths")
      ]

-- | A fee applied on a particular entity
newtype Fee = Fee
  { -- | Describes a numerical value in decimal form
    feePercentage :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Fee where
  parseJSON = genericParseJSON optionsFee

instance ToJSON Fee where
  toJSON = genericToJSON optionsFee

optionsFee :: Options
optionsFee =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("feePercentage", "percentage")
      ]

-- | Describes how a an order will be rendered/fulfilled to the end-customer
data Fulfillment = Fulfillment
  { -- |
    fulfillmentAgent :: Maybe Agent,
    -- |
    fulfillmentCustomer :: Maybe Customer,
    -- | Unique reference ID to the fulfillment of an order
    fulfillmentId :: Maybe Text,
    -- |
    fulfillmentState :: Maybe FulfillmentState,
    -- | The list of logical stops encountered during the fulfillment of an order.
    fulfillmentStops :: Maybe [Stop],
    -- |
    fulfillmentTags :: Maybe [TagGroup],
    -- | A code that describes the mode of fulfillment. This is typically set when there are multiple ways an order can be fulfilled. For example, a retail order can be fulfilled either via store pickup or a home delivery. Similarly, a medical consultation can be provided either in-person or via tele-consultation. The network policy must publish standard fulfillment type codes for the different modes of fulfillment.
    fulfillmentType :: Maybe Text,
    -- |
    fulfillmentVehicle :: Maybe Vehicle
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
      [ ("fulfillmentAgent", "agent"),
        ("fulfillmentCustomer", "customer"),
        ("fulfillmentId", "id"),
        ("fulfillmentState", "state"),
        ("fulfillmentStops", "stops"),
        ("fulfillmentTags", "tags"),
        ("fulfillmentType", "type"),
        ("fulfillmentVehicle", "vehicle")
      ]

-- | Describes the state of fulfillment
newtype FulfillmentState = FulfillmentState
  { -- |
    fulfillmentStateDescriptor :: Maybe Descriptor
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON FulfillmentState where
  parseJSON = genericParseJSON optionsFulfillmentState

instance ToJSON FulfillmentState where
  toJSON = genericToJSON optionsFulfillmentState

optionsFulfillmentState :: Options
optionsFulfillmentState =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fulfillmentStateDescriptor", "descriptor")
      ]

-- | Describes an image
data Image = Image
  { -- | Height of the image in pixels
    imageHeight :: Maybe Text,
    -- | The size of the image. The network policy can define the default dimensions of each type
    imageSizeType :: Maybe Text,
    -- | URL to the image. This can be a data url or an remote url
    imageUrl :: Maybe Text,
    -- | Width of the image in pixels
    imageWidth :: Maybe Text
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
      [ ("imageHeight", "height"),
        ("imageSizeType", "size_type"),
        ("imageUrl", "url"),
        ("imageWidth", "width")
      ]

-- |
data InitReq = InitReq
  { -- |
    initReqContext :: Context,
    -- |
    initReqMessage :: ConfirmReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON InitReq where
  parseJSON = genericParseJSON optionsInitReq

instance ToJSON InitReq where
  toJSON = genericToJSON optionsInitReq

optionsInitReq :: Options
optionsInitReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("initReqContext", "context"),
        ("initReqMessage", "message")
      ]

-- |
-- | The intent to buy or avail a product or a service. The BAP can declare the intent of the consumer containing &lt;ul&gt;&lt;li&gt;What they want (A product, service, offer)&lt;/li&gt;&lt;li&gt;Who they want (A seller, service provider, agent etc)&lt;/li&gt;&lt;li&gt;Where they want it and where they want it from&lt;/li&gt;&lt;li&gt;When they want it (start and end time of fulfillment&lt;/li&gt;&lt;li&gt;How they want to pay for it&lt;/li&gt;&lt;/ul&gt;&lt;br&gt;This has properties like descriptor,provider,fulfillment,payment,category,offer,item,tags&lt;br&gt;This is typically used by the BAP to send the purpose of the user&#39;s search to the BPP. This will be used by the BPP to find products or services it offers that may match the user&#39;s intent.&lt;br&gt;For example, in Mobility, the mobility consumer declares a mobility intent. In this case, the mobility consumer declares information that describes various aspects of their journey like,&lt;ul&gt;&lt;li&gt;Where would they like to begin their journey (intent.fulfillment.start.location)&lt;/li&gt;&lt;li&gt;Where would they like to end their journey (intent.fulfillment.end.location)&lt;/li&gt;&lt;li&gt;When would they like to begin their journey (intent.fulfillment.start.time)&lt;/li&gt;&lt;li&gt;When would they like to end their journey (intent.fulfillment.end.time)&lt;/li&gt;&lt;li&gt;Who is the transport service provider they would like to avail services from (intent.provider)&lt;/li&gt;&lt;li&gt;Who is traveling (This is not recommended in public networks) (intent.fulfillment.customer)&lt;/li&gt;&lt;li&gt;What kind of fare product would they like to purchase (intent.item)&lt;/li&gt;&lt;li&gt;What add-on services would they like to avail&lt;/li&gt;&lt;li&gt;What offers would they like to apply on their booking (intent.offer)&lt;/li&gt;&lt;li&gt;What category of services would they like to avail (intent.category)&lt;/li&gt;&lt;li&gt;What additional luggage are they carrying&lt;/li&gt;&lt;li&gt;How would they like to pay for their journey (intent.payment)&lt;/li&gt;&lt;/ul&gt;&lt;br&gt;For example, in health domain, a consumer declares the intent for a lab booking the describes various aspects of their booking like,&lt;ul&gt;&lt;li&gt;Where would they like to get their scan/test done (intent.fulfillment.start.location)&lt;/li&gt;&lt;li&gt;When would they like to get their scan/test done (intent.fulfillment.start.time)&lt;/li&gt;&lt;li&gt;When would they like to get the results of their test/scan (intent.fulfillment.end.time)&lt;/li&gt;&lt;li&gt;Who is the service provider they would like to avail services from (intent.provider)&lt;/li&gt;&lt;li&gt;Who is getting the test/scan (intent.fulfillment.customer)&lt;/li&gt;&lt;li&gt;What kind of test/scan would they like to purchase (intent.item)&lt;/li&gt;&lt;li&gt;What category of services would they like to avail (intent.category)&lt;/li&gt;&lt;li&gt;How would they like to pay for their journey (intent.payment)&lt;/li&gt;&lt;/ul&gt;
data Intent = Intent
  { -- |
    intentFulfillment :: Maybe Fulfillment,
    -- |
    intentPayment :: Maybe Payment,
    -- |
    intentTags :: Maybe [TagGroup]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Intent where
  parseJSON = genericParseJSON optionsIntent

instance ToJSON Intent where
  toJSON = genericToJSON optionsIntent

optionsIntent :: Options
optionsIntent =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("intentFulfillment", "fulfillment"),
        ("intentPayment", "payment"),
        ("intentTags", "tags")
      ]

-- | Describes a product or a service offered to the end consumer by the provider. In the mobility sector, it can represent a fare product like one way journey. In the logistics sector, it can represent the delivery service offering. In the retail domain it can represent a product like a grocery item.
data Item = Item
  { -- |
    itemDescriptor :: Maybe Descriptor,
    -- | Modes through which this item can be fulfilled
    itemFulfillmentIds :: Maybe [Text],
    -- | ID of the item.
    itemId :: Maybe Text,
    -- | Provider Locations this item is available in
    itemLocationIds :: Maybe [Text],
    -- | Payment modalities through which this item can be ordered
    itemPaymentIds :: Maybe [Text],
    -- |
    itemPrice :: Maybe Price,
    -- |
    itemTags :: Maybe [TagGroup]
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
      [ ("itemDescriptor", "descriptor"),
        ("itemFulfillmentIds", "fulfillment_ids"),
        ("itemId", "id"),
        ("itemLocationIds", "location_ids"),
        ("itemPaymentIds", "payment_ids"),
        ("itemPrice", "price"),
        ("itemTags", "tags")
      ]

-- | The physical location of something
data Location = Location
  { -- | Describes a postal address.
    locationAddress :: Maybe Text,
    -- |
    locationAreaCode :: Maybe Text,
    -- |
    locationCity :: Maybe City,
    -- |
    locationCountry :: Maybe Country,
    -- | Describes a GPS coordinate
    locationGps :: Maybe Text,
    -- |
    locationId :: Maybe Text,
    -- |
    locationState :: Maybe State
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Location where
  parseJSON = genericParseJSON optionsLocation

instance ToJSON Location where
  toJSON = genericToJSON optionsLocation

optionsLocation :: Options
optionsLocation =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("locationAddress", "address"),
        ("locationAreaCode", "area_code"),
        ("locationCity", "city"),
        ("locationCountry", "country"),
        ("locationGps", "gps"),
        ("locationId", "id"),
        ("locationState", "state")
      ]

-- |
data OnConfirmReq = OnConfirmReq
  { -- |
    onConfirmReqContext :: Context,
    -- |
    onConfirmReqError :: Maybe Error,
    -- |
    onConfirmReqMessage :: Maybe ConfirmReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OnConfirmReq where
  parseJSON = genericParseJSON optionsOnConfirmReq

instance ToJSON OnConfirmReq where
  toJSON = genericToJSON optionsOnConfirmReq

optionsOnConfirmReq :: Options
optionsOnConfirmReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("onConfirmReqContext", "context"),
        ("onConfirmReqError", "error"),
        ("onConfirmReqMessage", "message")
      ]

-- |
-- |
data OnInitReq = OnInitReq
  { -- |
    onInitReqContext :: Context,
    -- |
    onInitReqError :: Maybe Error,
    -- |
    onInitReqMessage :: Maybe ConfirmReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OnInitReq where
  parseJSON = genericParseJSON optionsOnInitReq

instance ToJSON OnInitReq where
  toJSON = genericToJSON optionsOnInitReq

optionsOnInitReq :: Options
optionsOnInitReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("onInitReqContext", "context"),
        ("onInitReqError", "error"),
        ("onInitReqMessage", "message")
      ]

-- |
-- |
data OnSearchReq = OnSearchReq
  { -- |
    onSearchReqContext :: Context,
    -- |
    onSearchReqError :: Maybe Error,
    -- |
    onSearchReqMessage :: Maybe OnSearchReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OnSearchReq where
  parseJSON = genericParseJSON optionsOnSearchReq

instance ToJSON OnSearchReq where
  toJSON = genericToJSON optionsOnSearchReq

optionsOnSearchReq :: Options
optionsOnSearchReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("onSearchReqContext", "context"),
        ("onSearchReqError", "error"),
        ("onSearchReqMessage", "message")
      ]

-- |
-- |
newtype OnSearchReqMessage = OnSearchReqMessage
  { -- |
    onSearchReqMessageCatalog :: Catalog
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OnSearchReqMessage where
  parseJSON = genericParseJSON optionsOnSearchReqMessage

instance ToJSON OnSearchReqMessage where
  toJSON = genericToJSON optionsOnSearchReqMessage

optionsOnSearchReqMessage :: Options
optionsOnSearchReqMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("onSearchReqMessageCatalog", "catalog")
      ]

-- |
data OnSelectReq = OnSelectReq
  { -- |
    onSelectReqContext :: Context,
    -- |
    onSelectReqError :: Maybe Error,
    -- |
    onSelectReqMessage :: Maybe OnSelectReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OnSelectReq where
  parseJSON = genericParseJSON optionsOnSelectReq

instance ToJSON OnSelectReq where
  toJSON = genericToJSON optionsOnSelectReq

optionsOnSelectReq :: Options
optionsOnSelectReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("onSelectReqContext", "context"),
        ("onSelectReqError", "error"),
        ("onSelectReqMessage", "message")
      ]

-- |
-- |
newtype OnSelectReqMessage = OnSelectReqMessage
  { -- |
    onSelectReqMessageOrder :: Maybe Order
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OnSelectReqMessage where
  parseJSON = genericParseJSON optionsOnSelectReqMessage

instance ToJSON OnSelectReqMessage where
  toJSON = genericToJSON optionsOnSelectReqMessage

optionsOnSelectReqMessage :: Options
optionsOnSelectReqMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("onSelectReqMessageOrder", "order")
      ]

-- |
data OnStatusReq = OnStatusReq
  { -- |
    onStatusReqContext :: Context,
    -- |
    onStatusReqError :: Maybe Error,
    -- |
    onStatusReqMessage :: Maybe ConfirmReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OnStatusReq where
  parseJSON = genericParseJSON optionsOnStatusReq

instance ToJSON OnStatusReq where
  toJSON = genericToJSON optionsOnStatusReq

optionsOnStatusReq :: Options
optionsOnStatusReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("onStatusReqContext", "context"),
        ("onStatusReqError", "error"),
        ("onStatusReqMessage", "message")
      ]

-- |
-- |
data OnTrackReq = OnTrackReq
  { -- |
    onTrackReqContext :: Context,
    -- |
    onTrackReqError :: Maybe Error,
    -- |
    onTrackReqMessage :: Maybe OnTrackReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OnTrackReq where
  parseJSON = genericParseJSON optionsOnTrackReq

instance ToJSON OnTrackReq where
  toJSON = genericToJSON optionsOnTrackReq

optionsOnTrackReq :: Options
optionsOnTrackReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("onTrackReqContext", "context"),
        ("onTrackReqError", "error"),
        ("onTrackReqMessage", "message")
      ]

-- |
-- |
data OnTrackReqMessage = OnTrackReqMessage
  { -- |
    onTrackReqMessageTracking :: Tracking
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OnTrackReqMessage where
  parseJSON = genericParseJSON optionsOnTrackReqMessage

instance ToJSON OnTrackReqMessage where
  toJSON = genericToJSON optionsOnTrackReqMessage

optionsOnTrackReqMessage :: Options
optionsOnTrackReqMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("onTrackReqMessageTracking", "tracking")
      ]

-- |
data OnUpdateReq = OnUpdateReq
  { -- |
    onUpdateReqContext :: Context,
    -- |
    onUpdateReqError :: Maybe Error,
    -- |
    onUpdateReqMessage :: Maybe ConfirmReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON OnUpdateReq where
  parseJSON = genericParseJSON optionsOnUpdateReq

instance ToJSON OnUpdateReq where
  toJSON = genericToJSON optionsOnUpdateReq

optionsOnUpdateReq :: Options
optionsOnUpdateReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("onUpdateReqContext", "context"),
        ("onUpdateReqError", "error"),
        ("onUpdateReqMessage", "message")
      ]

-- |
-- | Describes a selectable option
data Option = Option
  { -- |
    optionId :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Option where
  parseJSON = genericParseJSON optionsOption

instance ToJSON Option where
  toJSON = genericToJSON optionsOption

optionsOption :: Options
optionsOption =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("optionId", "id")
      ]

-- | Describes a legal purchase order. It contains the complete details of the legal contract created between the buyer and the seller.
data Order = Order
  { -- |
    orderBilling :: Maybe Billing,
    -- |
    orderCancellation :: Maybe Cancellation,
    -- | Cancellation terms of this item
    orderCancellationTerms :: Maybe [CancellationTerm],
    -- | The fulfillments involved in completing this order
    orderFulfillments :: Maybe [Fulfillment],
    -- | Human-readable ID of the order. This is generated at the BPP layer. The BPP can either generate order id within its system or forward the order ID created at the provider level.
    orderId :: Maybe Text,
    -- | The items purchased / availed in this order
    orderItems :: Maybe [Item],
    -- | The terms of settlement for this order
    orderPayments :: Maybe [Payment],
    -- |
    orderProvider :: Maybe Provider,
    -- |
    orderQuote :: Maybe Quotation,
    -- | Status of the order. Allowed values can be defined by the network policy
    orderStatus :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Order where
  parseJSON = genericParseJSON optionsOrder

instance ToJSON Order where
  toJSON = genericToJSON optionsOrder

optionsOrder :: Options
optionsOrder =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("orderBilling", "billing"),
        ("orderCancellation", "cancellation"),
        ("orderCancellationTerms", "cancellation_terms"),
        ("orderFulfillments", "fulfillments"),
        ("orderId", "id"),
        ("orderItems", "items"),
        ("orderPayments", "payments"),
        ("orderProvider", "provider"),
        ("orderQuote", "quote"),
        ("orderStatus", "status")
      ]

-- | Describes the terms of settlement between the BAP and the BPP for a single transaction. When instantiated, this object contains &lt;ol&gt;&lt;li&gt;the amount that has to be settled,&lt;/li&gt;&lt;li&gt;The payment destination destination details&lt;/li&gt;&lt;li&gt;When the settlement should happen, and&lt;/li&gt;&lt;li&gt;A transaction reference ID&lt;/li&gt;&lt;/ol&gt;. During a transaction, the BPP reserves the right to decide the terms of payment. However, the BAP can send its terms to the BPP first. If the BPP does not agree to those terms, it must overwrite the terms and return them to the BAP. If overridden, the BAP must either agree to the terms sent by the BPP in order to preserve the provider&#39;s autonomy, or abort the transaction. In case of such disagreements, the BAP and the BPP can perform offline negotiations on the payment terms. Once an agreement is reached, the BAP and BPP can resume transactions.
data Payment = Payment
  { -- | This field indicates who is the collector of payment. The BAP can set this value to 'bap' if it wants to collect the payment first and  settle it to the BPP. If the BPP agrees to those terms, the BPP should not send the payment url. Alternatively, the BPP can set this field with the value 'bpp' if it wants the payment to be made directly.
    paymentCollectedBy :: Maybe Text,
    -- | ID of the payment term that can be referred at an item or an order level in a catalog
    paymentId :: Maybe Text,
    -- |
    paymentParams :: Maybe PaymentParams,
    -- |
    paymentStatus :: Maybe Text,
    -- |
    paymentTags :: Maybe [TagGroup],
    -- |
    paymentType :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Payment where
  parseJSON = genericParseJSON optionsPayment

instance ToJSON Payment where
  toJSON = genericToJSON optionsPayment

optionsPayment :: Options
optionsPayment =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("paymentCollectedBy", "collected_by"),
        ("paymentId", "id"),
        ("paymentParams", "params"),
        ("paymentStatus", "status"),
        ("paymentTags", "tags"),
        ("paymentType", "type")
      ]

-- |
data PaymentParams = PaymentParams
  { -- |
    paymentParamsAmount :: Maybe Text,
    -- |
    paymentParamsBankAccountNumber :: Maybe Text,
    -- |
    paymentParamsBankCode :: Maybe Text,
    -- |
    paymentParamsCurrency :: Maybe Text,
    -- |
    paymentParamsVirtualPaymentAddress :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON PaymentParams where
  parseJSON = genericParseJSON optionsPaymentParams

instance ToJSON PaymentParams where
  toJSON = genericToJSON optionsPaymentParams

optionsPaymentParams :: Options
optionsPaymentParams =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("paymentParamsAmount", "amount"),
        ("paymentParamsBankAccountNumber", "bank_account_number"),
        ("paymentParamsBankCode", "bank_code"),
        ("paymentParamsCurrency", "currency"),
        ("paymentParamsVirtualPaymentAddress", "virtual_payment_address")
      ]

-- | Describes a person as any individual
data Person = Person
  { -- | Describes the identity of the person
    personId :: Maybe Text,
    -- |
    personImage :: Maybe Image,
    -- | the name of the person
    personName :: Maybe Text,
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
        ("personImage", "image"),
        ("personName", "name"),
        ("personTags", "tags")
      ]

-- | Describes the price of a product or service
data Price = Price
  { -- | Describes a numerical value in decimal form
    priceComputedValue :: Maybe Text,
    -- |
    priceCurrency :: Maybe Text,
    -- | Describes a numerical value in decimal form
    priceMaximumValue :: Maybe Text,
    -- | Describes a numerical value in decimal form
    priceMinimumValue :: Maybe Text,
    -- | Describes a numerical value in decimal form
    priceOfferedValue :: Maybe Text,
    -- | Describes a numerical value in decimal form
    priceValue :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Price where
  parseJSON = genericParseJSON optionsPrice

instance ToJSON Price where
  toJSON = genericToJSON optionsPrice

optionsPrice :: Options
optionsPrice =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("priceComputedValue", "computed_value"),
        ("priceCurrency", "currency"),
        ("priceMaximumValue", "maximum_value"),
        ("priceMinimumValue", "minimum_value"),
        ("priceOfferedValue", "offered_value"),
        ("priceValue", "value")
      ]

-- | Describes the catalog of a business.
data Provider = Provider
  { -- |
    providerDescriptor :: Maybe Descriptor,
    -- |
    providerFulfillments :: Maybe [Fulfillment],
    -- | Text of the provider
    providerId :: Maybe Text,
    -- |
    providerItems :: Maybe [Item],
    -- |
    providerLocations :: Maybe [Location],
    -- |
    providerPayments :: Maybe [Payment]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Provider where
  parseJSON = genericParseJSON optionsProvider

instance ToJSON Provider where
  toJSON = genericToJSON optionsProvider

optionsProvider :: Options
optionsProvider =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("providerDescriptor", "descriptor"),
        ("providerFulfillments", "fulfillments"),
        ("providerId", "id"),
        ("providerItems", "items"),
        ("providerLocations", "locations"),
        ("providerPayments", "payments")
      ]

-- | Describes a quote. It is the estimated price of products or services from the BPP.&lt;br&gt;This has properties like price, breakup, ttl
data Quotation = Quotation
  { -- | the breakup of the total quoted price
    quotationBreakup :: Maybe [QuotationBreakupInner],
    -- |
    quotationPrice :: Maybe Price,
    -- | Describes duration as per ISO8601 format
    quotationTtl :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Quotation where
  parseJSON = genericParseJSON optionsQuotation

instance ToJSON Quotation where
  toJSON = genericToJSON optionsQuotation

optionsQuotation :: Options
optionsQuotation =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("quotationBreakup", "breakup"),
        ("quotationPrice", "price"),
        ("quotationTtl", "ttl")
      ]

-- |
data QuotationBreakupInner = QuotationBreakupInner
  { -- |
    quotationBreakupInnerPrice :: Maybe Price,
    -- |
    quotationBreakupInnerTitle :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON QuotationBreakupInner where
  parseJSON = genericParseJSON optionsQuotationBreakupInner

instance ToJSON QuotationBreakupInner where
  toJSON = genericToJSON optionsQuotationBreakupInner

optionsQuotationBreakupInner :: Options
optionsQuotationBreakupInner =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("quotationBreakupInnerPrice", "price"),
        ("quotationBreakupInnerTitle", "title")
      ]

-- | Describes the rating of an entity
data Rating = Rating
  { -- | Text of the object being rated
    ratingId :: Maybe Text,
    -- | Category of the entity being rated
    ratingRatingCategory :: Maybe Text,
    -- | Rating value given to the object. This can be a single value or can also contain an inequality operator like gt, gte, lt, lte. This can also contain an inequality expression containing logical operators like && and ||.
    ratingValue :: Maybe Text,
    ratingFeedbackForm :: Maybe [FeedbackForm]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Rating where
  parseJSON = genericParseJSON optionsRating

instance ToJSON Rating where
  toJSON = genericToJSON optionsRating

optionsRating :: Options
optionsRating =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("ratingId", "id"),
        ("ratingRatingCategory", "rating_category"),
        ("ratingValue", "value")
      ]

data FeedbackForm = FeedbackForm
  { feedbackFormQuestion :: Text,
    feedbackFormAnswer :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON FeedbackForm where
  parseJSON = genericParseJSON optionsFeedbackForm

instance ToJSON FeedbackForm where
  toJSON = genericToJSON optionsFeedbackForm

optionsFeedbackForm :: Options
optionsFeedbackForm =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("feedbackFormQuestion", "question"),
        ("feedbackFormAnswer", "answer")
      ]

-- |
data RatingReq = RatingReq
  { -- |
    ratingReqContext :: Context,
    -- |
    ratingReqMessage :: RatingReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RatingReq where
  parseJSON = genericParseJSON optionsRatingReq

instance ToJSON RatingReq where
  toJSON = genericToJSON optionsRatingReq

optionsRatingReq :: Options
optionsRatingReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("ratingReqContext", "context"),
        ("ratingReqMessage", "message")
      ]

-- |
-- |
data RatingReqMessage = RatingReqMessage
  { -- |
    ratingReqMessageRatings :: Maybe [Rating]
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON RatingReqMessage where
  parseJSON = genericParseJSON optionsRatingReqMessage

instance ToJSON RatingReqMessage where
  toJSON = genericToJSON optionsRatingReqMessage

optionsRatingReqMessage :: Options
optionsRatingReqMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("ratingReqMessageRatings", "ratings")
      ]

-- |
data SearchReq = SearchReq
  { -- |
    searchReqContext :: Context,
    -- |
    searchReqMessage :: SearchReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON SearchReq where
  parseJSON = genericParseJSON optionsSearchReq

instance ToJSON SearchReq where
  toJSON = genericToJSON optionsSearchReq

optionsSearchReq :: Options
optionsSearchReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("searchReqContext", "context"),
        ("searchReqMessage", "message")
      ]

-- |
-- |
newtype SearchReqMessage = SearchReqMessage
  { -- |
    searchReqMessageIntent :: Maybe Intent
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON SearchReqMessage where
  parseJSON = genericParseJSON optionsSearchReqMessage

instance ToJSON SearchReqMessage where
  toJSON = genericToJSON optionsSearchReqMessage

optionsSearchReqMessage :: Options
optionsSearchReqMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("searchReqMessageIntent", "intent")
      ]

-- |
data SelectReq = SelectReq
  { -- |
    selectReqContext :: Context,
    -- |
    selectReqMessage :: ConfirmReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON SelectReq where
  parseJSON = genericParseJSON optionsSelectReq

instance ToJSON SelectReq where
  toJSON = genericToJSON optionsSelectReq

optionsSelectReq :: Options
optionsSelectReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("selectReqContext", "context"),
        ("selectReqMessage", "message")
      ]

-- |
-- | A bounded geopolitical region of governance inside a country.
newtype State = State
  { -- | Name of the state
    stateName :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON State where
  parseJSON = genericParseJSON optionsState

instance ToJSON State where
  toJSON = genericToJSON optionsState

optionsState :: Options
optionsState =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("stateName", "name")
      ]

-- |
data StatusReq = StatusReq
  { -- |
    statusReqContext :: Context,
    -- |
    statusReqMessage :: StatusReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON StatusReq where
  parseJSON = genericParseJSON optionsStatusReq

instance ToJSON StatusReq where
  toJSON = genericToJSON optionsStatusReq

optionsStatusReq :: Options
optionsStatusReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("statusReqContext", "context"),
        ("statusReqMessage", "message")
      ]

-- |
-- |
newtype StatusReqMessage = StatusReqMessage
  { -- |
    statusReqMessageRefId :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON StatusReqMessage where
  parseJSON = genericParseJSON optionsStatusReqMessage

instance ToJSON StatusReqMessage where
  toJSON = genericToJSON optionsStatusReqMessage

optionsStatusReqMessage :: Options
optionsStatusReqMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("statusReqMessageRefId", "ref_id")
      ]

-- | A logical point in space and time during the fulfillment of an order.
data Stop = Stop
  { -- |
    stopAuthorization :: Maybe Authorization,
    -- |
    stopLocation :: Maybe Location,
    -- |
    stopTime :: Maybe Time,
    -- | The type of stop. Allowed values of this property can be defined by the network policy.
    stopType :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Stop where
  parseJSON = genericParseJSON optionsStop

instance ToJSON Stop where
  toJSON = genericToJSON optionsStop

optionsStop :: Options
optionsStop =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("stopAuthorization", "authorization"),
        ("stopLocation", "location"),
        ("stopTime", "time"),
        ("stopType", "type")
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
      [ ("tagGroupDescriptor", "descriptor"),
        ("tagGroupDisplay", "display"),
        ("tagGroupList", "list")
      ]

-- | Describes time in its various forms. It can be a single point in time; duration; or a structured timetable of operations&lt;br&gt;This has properties like label, time stamp,duration,range, days, schedule
data Time = Time
  { -- |
    timeTimestamp :: Maybe UTCTime
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
      [ ("timeTimestamp", "timestamp")
      ]

-- |
data TrackReq = TrackReq
  { -- |
    trackReqContext :: Context,
    -- |
    trackReqMessage :: TrackReqMessage
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON TrackReq where
  parseJSON = genericParseJSON optionsTrackReq

instance ToJSON TrackReq where
  toJSON = genericToJSON optionsTrackReq

optionsTrackReq :: Options
optionsTrackReq =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("trackReqContext", "context"),
        ("trackReqMessage", "message")
      ]

-- |
-- |
data TrackReqMessage = TrackReqMessage
  { -- |
    trackReqMessageOrderId :: Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON TrackReqMessage where
  parseJSON = genericParseJSON optionsTrackReqMessage

instance ToJSON TrackReqMessage where
  toJSON = genericToJSON optionsTrackReqMessage

optionsTrackReqMessage :: Options
optionsTrackReqMessage =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("trackReqMessageOrderId", "order_id")
      ]

-- | Contains tracking information that can be used by the BAP to track the fulfillment of an order in real-time. which is useful for knowing the location of time sensitive deliveries.
data Tracking = Tracking
  { -- | This value indicates if the tracking is currently active or not. If this value is `active`, then the BAP can begin tracking the order. If this value is `inactive`, the tracking URL is considered to be expired and the BAP should stop tracking the order.
    trackingStatus :: Maybe Text,
    -- | A URL to the tracking endpoint. This can be a link to a tracking webpage, a webhook URL created by the BAP where BPP can push the tracking data, or a GET url creaed by the BPP which the BAP can poll to get the tracking data. It can also be a websocket URL where the BPP can push real-time tracking data.
    trackingUrl :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Tracking where
  parseJSON = genericParseJSON optionsTracking

instance ToJSON Tracking where
  toJSON = genericToJSON optionsTracking

optionsTracking :: Options
optionsTracking =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("trackingStatus", "status"),
        ("trackingUrl", "url")
      ]

-- | Describes a vehicle is a device that is designed or used to transport people or cargo over land, water, air, or through space.&lt;br&gt;This has properties like category, capacity, make, model, size,variant,color,energy_type,registration
data Vehicle = Vehicle
  { -- |
    vehicleCategory :: Maybe Text,
    -- |
    vehicleColor :: Maybe Text,
    -- |
    vehicleMake :: Maybe Text,
    -- |
    vehicleModel :: Maybe Text,
    -- |
    vehicleRegistration :: Maybe Text,
    -- |
    vehicleVariant :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Vehicle where
  parseJSON = genericParseJSON optionsVehicle

instance ToJSON Vehicle where
  toJSON = genericToJSON optionsVehicle

optionsVehicle :: Options
optionsVehicle =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("vehicleCategory", "category"),
        ("vehicleColor", "color"),
        ("vehicleMake", "make"),
        ("vehicleModel", "model"),
        ("vehicleRegistration", "registration"),
        ("vehicleVariant", "variant")
      ]
