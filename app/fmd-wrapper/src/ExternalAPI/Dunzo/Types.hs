{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module ExternalAPI.Dunzo.Types where

import Beckn.Types.Common
import Beckn.Types.Error.APIError
import Beckn.Types.Error.FromResponse
import Data.Aeson hiding (Error)
import Data.Char (toLower)
import EulerHS.Prelude
import Servant (FromHttpApiData, ToHttpApiData)
import qualified Types.Beckn.Error as Beckn
import Types.Common

newtype TaskId = TaskId {getTaskId :: Text}
  deriving (Generic, Show)

deriving newtype instance ToJSON TaskId

deriving newtype instance FromJSON TaskId

deriving newtype instance ToHttpApiData TaskId

deriving newtype instance FromHttpApiData TaskId

data TokenReq = TokenReq
  { client_id :: ClientId,
    client_secret :: ClientSecret
  }
  deriving (Generic, Show)

newtype TokenRes = TokenRes
  {token :: Token}
  deriving (Show, Generic)

instance ToJSON TokenRes where
  toJSON = genericToJSON defaultOptions

instance FromJSON TokenRes where
  parseJSON = genericParseJSON defaultOptions

data QuoteReq = QuoteReq
  { pickup_lat :: Double,
    pickup_lng :: Double,
    drop_lat :: Double,
    drop_lng :: Double,
    category_id :: Text
  }
  deriving (Generic, Show)

data QuoteRes = QuoteRes
  { category_id :: Text,
    distance :: Float,
    eta :: Eta,
    estimated_price :: Float
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Eta = Eta
  { pickup :: Maybe Float, -- minutes
    dropoff :: Float -- minutes
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data LocationDetails = LocationDetails
  { lat :: Double,
    lng :: Double,
    address :: Address
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Address = Address
  { apartment_address :: Maybe Text,
    street_address_1 :: Text,
    street_address_2 :: Text,
    landmark :: Maybe Text,
    city :: Maybe Text,
    state :: Text,
    pincode :: Maybe Text,
    country :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data PersonDetails = PersonDetails
  { name :: Text,
    phone_number :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data LatLng = LatLng {lat :: Double, lng :: Double}
  deriving (Show, Generic, ToJSON, FromJSON)

data RunnerDetails = RunnerDetails
  { name :: Text,
    phone_number :: Text,
    location :: LatLng
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype PackageContent = PackageContent {content :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, ToJSON, FromJSON)

dzPackageContentList :: [PackageContent]
dzPackageContentList =
  [ "Documents | Books",
    "Clothes | Accessories",
    "Food | Flowers",
    "Household Items",
    "Sports & Other Equipment",
    "Electronic Items"
  ]

data TaskState
  = CREATED
  | QUEUED
  | RUNNER_ACCEPTED
  | REACHED_FOR_PICKUP
  | PICKUP_COMPLETE
  | STARTED_FOR_DELIVERY
  | REACHED_FOR_DELIVERY
  | DELIVERED
  | CANCELLED
  | RUNNER_CANCELLED
  deriving (Show, Generic)

data TaskStatus = TaskStatus
  { task_id :: TaskId,
    state :: TaskState,
    eta :: Maybe Eta,
    estimated_price :: Maybe Float,
    event_timestamp :: Maybe Integer,
    request_timestamp :: Maybe Integer,
    tracking_url :: Maybe Text,
    runner :: Maybe RunnerDetails,
    price :: Maybe Float,
    total_time :: Maybe Float, -- minutes
    cancelled_by :: Maybe Text,
    cancellation_reason :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data CreateTaskReq = CreateTaskReq
  { request_id :: Text,
    pickup_details :: LocationDetails,
    drop_details :: LocationDetails,
    sender_details :: PersonDetails,
    receiver_details :: PersonDetails,
    special_instructions :: Maybe Text,
    package_approx_value :: Maybe Float,
    package_content :: [PackageContent],
    reference_id :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON CreateTaskReq where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

instance FromJSON CreateTaskReq where
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

type CreateTaskRes = TaskStatus

newtype CancelTaskReq = CancelTaskReq {cancellation_reason :: Text}
  deriving (Show, Generic)

instance ToJSON CancelTaskReq where
  toJSON = genericToJSON defaultOptions

instance FromJSON CancelTaskReq where
  parseJSON = genericParseJSON defaultOptions

taskStateOptions :: Options
taskStateOptions = defaultOptions {constructorTagModifier = map toLower}

instance ToJSON TaskState where
  toJSON = genericToJSON taskStateOptions

instance FromJSON TaskState where
  parseJSON = genericParseJSON taskStateOptions

data Error = Error
  { code :: Text,
    message :: Text,
    details :: Value
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance FromResponse Error where
  fromResponse = fromJsonResponse

instance ToBeckn Beckn.Error Error where
  toBeckn Error {..} =
    Beckn.Error
      { _type = Beckn.DOMAIN_ERROR,
        code = dunzoCodeToBecknCode code,
        path = Nothing,
        message = Just message
      }

dunzoCodeToBecknCode :: Text -> Text
dunzoCodeToBecknCode = \case
  "unauthorized" -> "CORE001"
  "rate_limit_exceeded" -> "CORE002"
  "validation_failed" -> "CORE003"
  "bad_request" -> "CORE003"
  "unserviceable_location_error" -> "FMD001"
  "stock_out_error" -> "FMD009"
  "internal_server_error" -> "CORE002"
  "duplicate_request" -> "CORE003"
  "rain_error" -> "FMD009"
  "different_city_error" -> "FMD001"
  "near_by_location_error" -> "FMD001"
  "service_unavailable" -> "CORE002"
  "default" -> "CORE003"
  _ -> "CORE003"

-- TODO: figure out all the codes
dunzoCodeToHttpCode :: Text -> HttpCode
dunzoCodeToHttpCode = \case
  "unauthorized" -> E401
  "bad_request" -> E400
  "service_unavailable" -> E503
  _ -> E500

-- TODO: make it a beckn-specific error
instance IsAPIError Error where
  toErrorCode Error {code} = dunzoCodeToBecknCode code
  toHttpCode Error {code} = dunzoCodeToHttpCode code
  toMessage Error {message} = Just message

instanceExceptionWithParent 'APIException ''Error
