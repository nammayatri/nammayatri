{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module External.Dunzo.Types where

import Beckn.Types.Common
import qualified Beckn.Types.Core.Error as Beckn
import Data.Aeson hiding (Error)
import Data.Char (toLower)
import qualified Data.Text as T
import EulerHS.Prelude
import Servant (FromHttpApiData, ToHttpApiData)

newtype ClientId = ClientId {getClientId :: Text}
  deriving (Generic, Show)

deriving newtype instance ToJSON ClientId

deriving newtype instance FromJSON ClientId

deriving newtype instance ToHttpApiData ClientId

deriving newtype instance FromHttpApiData ClientId

newtype ClientSecret = ClientSecret {getClientSecret :: Text}
  deriving (Generic, Show)

deriving newtype instance ToJSON ClientSecret

deriving newtype instance FromJSON ClientSecret

deriving newtype instance ToHttpApiData ClientSecret

deriving newtype instance FromHttpApiData ClientSecret

newtype TaskId = TaskId {getTaskId :: Text}
  deriving (Generic, Show)

deriving newtype instance ToJSON TaskId

deriving newtype instance FromJSON TaskId

deriving newtype instance ToHttpApiData TaskId

deriving newtype instance FromHttpApiData TaskId

newtype Token = Token {getToken :: Text}
  deriving (Generic, Show)

deriving newtype instance ToJSON Token

deriving newtype instance FromJSON Token

deriving newtype instance ToHttpApiData Token

deriving newtype instance FromHttpApiData Token

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
  { pickup :: Maybe Integer, -- minutes
    dropoff :: Integer -- minutes
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

-- FIXME: Rewrite packageContentOptions and drop this
{-# HLINT ignore PackageContent #-}
data PackageContent
  = Documents_or_Books
  | Clothes_or_Accessories
  | Food_or_Flowers
  | Household_Items
  | Sports_and_Other_Equipment
  | Electronic_Items
  deriving (Show, Generic)

dzPackageContentList :: [PackageContent]
dzPackageContentList = [Documents_or_Books, Clothes_or_Accessories, Food_or_Flowers, Household_Items, Sports_and_Other_Equipment, Electronic_Items]

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

packageContentOptions :: Options
packageContentOptions =
  defaultOptions
    { constructorTagModifier = T.unpack . T.replace "_" " " . T.replace "_and_" " & " . T.replace "_or_" " | " . T.pack
    }

taskStateOptions :: Options
taskStateOptions = defaultOptions {constructorTagModifier = map toLower}

instance ToJSON PackageContent where
  toJSON = genericToJSON packageContentOptions

instance FromJSON PackageContent where
  parseJSON = genericParseJSON packageContentOptions

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

instance ToBeckn Beckn.Error Error where
  toBeckn Error {..} =
    Beckn.Error
      { _type = "DOMAIN-ERROR",
        _code = becknErrCode,
        _path = Nothing,
        _message = Just message
      }
    where
      becknErrCode = case code of
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
