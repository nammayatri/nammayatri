{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module BecknV2.FRFS.Types
  ( Ack (..),
    AckMessage (..),
    AckResponse (..),
    Authorization (..),
    Billing (..),
    CancellationTerm (..),
    Catalog (..),
    Category (..),
    City (..),
    ConfirmReq (..),
    ConfirmReqMessage (..),
    Context (..),
    Country (..),
    Descriptor (..),
    Domain (..),
    Error (..),
    Fulfillment (..),
    Image (..),
    InitReq (..),
    Intent (..),
    Item (..),
    ItemQuantity (..),
    ItemQuantityMaximum (..),
    ItemQuantityMinimum (..),
    ItemQuantitySelected (..),
    Location (..),
    MediaFile (..),
    OnConfirmReq (..),
    OnInitReq (..),
    OnSearchReq (..),
    OnSearchReqMessage (..),
    OnStatusReq (..),
    Order (..),
    Payment (..),
    PaymentParams (..),
    Price (..),
    Provider (..),
    Quotation (..),
    QuotationBreakupInner (..),
    SearchReq (..),
    SearchReqMessage (..),
    StatusReq (..),
    StatusReqMessage (..),
    Stop (..),
    Tag (..),
    TagGroup (..),
    Time (..),
    TimeRange (..),
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

-- | Describes an authorization mechanism used to start or end the fulfillment of an order. For example, in the mobility sector, the driver may require a one-time password to initiate the ride. In the healthcare sector, a patient may need to provide a password to open a video conference link during a teleconsultation.
data Authorization = Authorization
  { -- | Status of the token
    authorizationStatus :: Maybe Text,
    -- | Token used for authorization. This is typically generated at the BPP. The BAP can send this value to the user via any channel that it uses to authenticate the user like SMS, Email, Push notification, or in-app rendering.
    authorizationToken :: Maybe Text,
    -- | Type of authorization mechanism used. The allowed values for this field can be published as part of the network policy.
    authorizationType :: Maybe Text,
    -- | Timestamp in RFC3339 format until which token is valid
    authorizationValidTo :: Maybe UTCTime
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
      [ ("authorizationStatus", "status"),
        ("authorizationToken", "token"),
        ("authorizationType", "type"),
        ("authorizationValidTo", "valid_to")
      ]

-- | Describes the billing details of an entity.&lt;br&gt;This has properties like name,organization,address,email,phone,time,tax_number, created_at,updated_at
data Billing = Billing
  { -- | Email address where the bill is sent to
    billingEmail :: Maybe Text,
    -- | Name of the billable entity
    billingName :: Maybe Text,
    -- | Phone number of the billable entity
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
      [ ("billingEmail", "email"),
        ("billingName", "name"),
        ("billingPhone", "phone")
      ]

-- | Describes the cancellation terms of an item or an order. This can be referenced at an item or order level. Item-level cancellation terms can override the terms at the order level.
data CancellationTerm = CancellationTerm
  { -- |
    cancellationTermExternalRef :: Maybe MediaFile
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
      [ ("cancellationTermExternalRef", "external_ref")
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

-- | A label under which a collection of items can be grouped.
data Category = Category
  { -- |
    categoryDescriptor :: Maybe Descriptor,
    -- | ID of the category
    categoryId :: Maybe Text
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON Category where
  parseJSON = genericParseJSON optionsCategory

instance ToJSON Category where
  toJSON = genericToJSON optionsCategory

optionsCategory :: Options
optionsCategory =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("categoryDescriptor", "descriptor"),
        ("categoryId", "id")
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
data ConfirmReqMessage = ConfirmReqMessage
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
    contextMessageId :: Maybe Text,
    -- | Time of request generation in RFC3339 format
    contextTimestamp :: Maybe UTCTime,
    -- | This is a unique value which persists across all API calls from `search` through `confirm`. This is done to indicate an active user session across multiple requests. The BPPs can use this value to push personalized recommendations, and dynamic offerings related to an ongoing transaction despite being unaware of the user active on the BAP.
    contextTransactionId :: Maybe Text,
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

-- | Physical description of something.
data Descriptor = Descriptor
  { -- |
    descriptorCode :: Maybe Text,
    -- |
    descriptorImages :: Maybe [Image],
    -- |
    descriptorName :: Maybe Text
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
        ("descriptorImages", "images"),
        ("descriptorName", "name")
      ]

-- | Described the industry sector or sub-sector. The network policy should contain codes for all the industry sectors supported by the network. Domains can be created in varying levels of granularity. The granularity of a domain can be decided by the participants of the network. Too broad domains will result in irrelevant search broadcast calls to BPPs that don&#39;t have services supporting the domain. Too narrow domains will result in a large number of registry entries for each BPP. It is recommended that network facilitators actively collaborate with various working groups and network participants to carefully choose domain codes keeping in mind relevance, performance, and opportunity cost. It is recommended that networks choose broad domains like mobility, logistics, healthcare etc, and progressively granularize them as and when the number of network participants for each domain grows large.
data Domain = Domain
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

-- | Describes how a an order will be rendered/fulfilled to the end-customer
data Fulfillment = Fulfillment
  { -- | Unique reference ID to the fulfillment of an order
    fulfillmentId :: Maybe Text,
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
      [ ("fulfillmentId", "id"),
        ("fulfillmentStops", "stops"),
        ("fulfillmentTags", "tags"),
        ("fulfillmentType", "type"),
        ("fulfillmentVehicle", "vehicle")
      ]

-- | Describes an image
data Image = Image
  { -- | URL to the image. This can be a data url or an remote url
    imageUrl :: Maybe Text
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
      [ ("imageUrl", "url")
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
    intentPayment :: Maybe Payment
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
        ("intentPayment", "payment")
      ]

-- | Describes a product or a service offered to the end consumer by the provider. In the mobility sector, it can represent a fare product like one way journey. In the logistics sector, it can represent the delivery service offering. In the retail domain it can represent a product like a grocery item.
data Item = Item
  { -- | Categories this item can be listed under
    itemCategoryIds :: Maybe [Text],
    -- |
    itemDescriptor :: Maybe Descriptor,
    -- | Modes through which this item can be fulfilled
    itemFulfillmentIds :: Maybe [Text],
    -- | ID of the item.
    itemId :: Maybe Text,
    -- |
    itemPrice :: Maybe Price,
    -- |
    itemQuantity :: Maybe ItemQuantity,
    -- |
    itemTime :: Maybe Time
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
      [ ("itemCategoryIds", "category_ids"),
        ("itemDescriptor", "descriptor"),
        ("itemFulfillmentIds", "fulfillment_ids"),
        ("itemId", "id"),
        ("itemPrice", "price"),
        ("itemQuantity", "quantity"),
        ("itemTime", "time")
      ]

-- | Describes the count or amount of an item
data ItemQuantity = ItemQuantity
  { -- |
    itemQuantityMaximum :: Maybe ItemQuantityMaximum,
    -- |
    itemQuantityMinimum :: Maybe ItemQuantityMinimum,
    -- |
    itemQuantitySelected :: Maybe ItemQuantitySelected
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ItemQuantity where
  parseJSON = genericParseJSON optionsItemQuantity

instance ToJSON ItemQuantity where
  toJSON = genericToJSON optionsItemQuantity

optionsItemQuantity :: Options
optionsItemQuantity =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("itemQuantityMaximum", "maximum"),
        ("itemQuantityMinimum", "minimum"),
        ("itemQuantitySelected", "selected")
      ]

-- | This represents the maximum quantity allowed for purchase of the item
data ItemQuantityMaximum = ItemQuantityMaximum
  { -- |
    itemQuantityMaximumCount :: Maybe Int
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ItemQuantityMaximum where
  parseJSON = genericParseJSON optionsItemQuantityMaximum

instance ToJSON ItemQuantityMaximum where
  toJSON = genericToJSON optionsItemQuantityMaximum

optionsItemQuantityMaximum :: Options
optionsItemQuantityMaximum =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("itemQuantityMaximumCount", "count")
      ]

-- | This represents the minimum quantity allowed for purchase of the item
data ItemQuantityMinimum = ItemQuantityMinimum
  { -- |
    itemQuantityMinimumCount :: Maybe Int
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ItemQuantityMinimum where
  parseJSON = genericParseJSON optionsItemQuantityMinimum

instance ToJSON ItemQuantityMinimum where
  toJSON = genericToJSON optionsItemQuantityMinimum

optionsItemQuantityMinimum :: Options
optionsItemQuantityMinimum =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("itemQuantityMinimumCount", "count")
      ]

-- | This represents the quantity selected for purchase of the item
data ItemQuantitySelected = ItemQuantitySelected
  { -- |
    itemQuantitySelectedCount :: Maybe Int
  }
  deriving (Show, Eq, Generic, Data)

instance FromJSON ItemQuantitySelected where
  parseJSON = genericParseJSON optionsItemQuantitySelected

instance ToJSON ItemQuantitySelected where
  toJSON = genericToJSON optionsItemQuantitySelected

optionsItemQuantitySelected :: Options
optionsItemQuantitySelected =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("itemQuantitySelectedCount", "count")
      ]

-- | The physical location of something
data Location = Location
  { -- |
    locationDescriptor :: Maybe Descriptor,
    -- | Describes a GPS coordinate
    locationGps :: Maybe Text,
    -- |
    locationCity :: Maybe City,
    -- |
    locationCountry :: Maybe Country
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
      [ ("locationDescriptor", "descriptor"),
        ("locationCity", "city"),
        ("locationCountry", "country"),
        ("locationGps", "gps")
      ]

-- | This object contains a url to a media file.
data MediaFile = MediaFile
  { -- | indicates the nature and format of the document, file, or assortment of bytes. MIME types are defined and standardized in IETF's RFC 6838
    mediaFileMimetype :: Maybe Text,
    -- | The URL of the file
    mediaFileUrl :: Maybe Text
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
        ("mediaFileUrl", "url")
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
data OnSearchReqMessage = OnSearchReqMessage
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
-- | Describes a legal purchase order. It contains the complete details of the legal contract created between the buyer and the seller.
data Order = Order
  { -- |
    orderBilling :: Maybe Billing,
    -- | Cancellation terms of this item
    orderCancellationTerms :: Maybe [CancellationTerm],
    -- | The date-time of creation of this order
    orderCreatedAt :: Maybe UTCTime,
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
    orderStatus :: Maybe Text,
    -- |
    orderTags :: Maybe [TagGroup],
    -- | The date-time of updated of this order
    orderUpdatedAt :: Maybe UTCTime
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
        ("orderCancellationTerms", "cancellation_terms"),
        ("orderCreatedAt", "created_at"),
        ("orderFulfillments", "fulfillments"),
        ("orderId", "id"),
        ("orderItems", "items"),
        ("orderPayments", "payments"),
        ("orderProvider", "provider"),
        ("orderQuote", "quote"),
        ("orderStatus", "status"),
        ("orderTags", "tags"),
        ("orderUpdatedAt", "updated_at")
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
    -- | The reference transaction ID associated with a payment activity
    paymentParamsTransactionId :: Maybe Text,
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
        ("paymentParamsTransactionId", "transaction_id"),
        ("paymentParamsVirtualPaymentAddress", "virtual_payment_address")
      ]

-- | Describes the price of a product or service
data Price = Price
  { -- |
    priceCurrency :: Maybe Text,
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
      [ ("priceCurrency", "currency"),
        ("priceValue", "value")
      ]

-- | Describes the catalog of a business.
data Provider = Provider
  { -- |
    providerCategories :: Maybe [Category],
    -- |
    providerDescriptor :: Maybe Descriptor,
    -- |
    providerFulfillments :: Maybe [Fulfillment],
    -- | Text of the provider
    providerId :: Maybe Text,
    -- |
    providerItems :: Maybe [Item],
    -- |
    providerPayments :: Maybe [Payment],
    -- |
    providerTags :: Maybe [TagGroup],
    -- |
    providerTime :: Maybe Time
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
      [ ("providerCategories", "categories"),
        ("providerDescriptor", "descriptor"),
        ("providerFulfillments", "fulfillments"),
        ("providerId", "id"),
        ("providerItems", "items"),
        ("providerPayments", "payments"),
        ("providerTags", "tags"),
        ("providerTime", "time")
      ]

-- | Describes a quote. It is the estimated price of products or services from the BPP.&lt;br&gt;This has properties like price, breakup, ttl
data Quotation = Quotation
  { -- | the breakup of the total quoted price
    quotationBreakup :: Maybe [QuotationBreakupInner],
    -- |
    quotationPrice :: Maybe Price
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
        ("quotationPrice", "price")
      ]

-- |
data QuotationBreakupInner = QuotationBreakupInner
  { -- |
    quotationBreakupInnerItem :: Maybe Item,
    -- |
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
      [ ("quotationBreakupInnerItem", "item"),
        ("quotationBreakupInnerPrice", "price"),
        ("quotationBreakupInnerTitle", "title")
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
data SearchReqMessage = SearchReqMessage
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
data StatusReqMessage = StatusReqMessage
  { -- |
    statusReqMessageOrderId :: Text
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
      [ ("statusReqMessageOrderId", "order_id")
      ]

-- | A logical point in space and time during the fulfillment of an order.
data Stop = Stop
  { -- |
    stopAuthorization :: Maybe Authorization,
    -- |
    stopId :: Maybe Text,
    -- |
    stopInstructions :: Maybe Descriptor,
    -- |
    stopLocation :: Maybe Location,
    -- |
    stopParentStopId :: Maybe Text,
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
        ("stopId", "id"),
        ("stopInstructions", "instructions"),
        ("stopLocation", "location"),
        ("stopParentStopId", "parent_stop_id"),
        ("stopType", "type")
      ]

-- | Describes a tag. This is used to contain extended metadata. This object can be added as a property to any schema to describe extended attributes. For BAPs, tags can be sent during search to optimize and filter search results. BPPs can use tags to index their catalog to allow better search functionality. Tags are sent by the BPP as part of the catalog response in the &#x60;on_search&#x60; callback. Tags are also meant for display purposes. Upon receiving a tag, BAPs are meant to render them as name-value pairs. This is particularly useful when rendering tabular information about a product or service.
data Tag = Tag
  { -- |
    tagDescriptor :: Maybe Descriptor,
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
  { -- | Describes duration as per ISO8601 format
    timeDuration :: Maybe Text,
    -- |
    timeLabel :: Maybe Text,
    -- |
    timeRange :: Maybe TimeRange
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
      [ ("timeDuration", "duration"),
        ("timeLabel", "label"),
        ("timeRange", "range")
      ]

-- |
data TimeRange = TimeRange
  { -- |
    timeRangeEnd :: Maybe UTCTime,
    -- |
    timeRangeStart :: Maybe UTCTime
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
      [ ("timeRangeEnd", "end"),
        ("timeRangeStart", "start")
      ]

-- | Describes a vehicle is a device that is designed or used to transport people or cargo over land, water, air, or through space.&lt;br&gt;This has properties like category, capacity, make, model, size,variant,color,energy_type,registration
data Vehicle = Vehicle
  { -- |
    vehicleCategory :: Maybe Text
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
      [ ("vehicleCategory", "category")
      ]
