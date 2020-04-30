module Beckn.Types.API.Quota where

import           Beckn.Types.Common
import           Beckn.Types.Storage.Quota
import           Data.Default
import           Data.Swagger
import           Data.Time.LocalTime
import           EulerHS.Prelude
----------
-- Create
----------
data CreateReq =
  CreateReq
  { _maxAllowed :: Int
  , _quota_type :: QuotaType
  , _EntityId   :: Text
  , _entityType :: EntityType
  , _startTime  :: LocalTime
  , _endTime    :: LocalTime
  }
  deriving (Show, Generic, ToJSON, ToSchema)

instance FromJSON CreateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions


data CreateRes =
  CreateRes
  { _quota :: Quota
  }
  deriving (Show, Generic, FromJSON, Default, ToSchema)

instance ToJSON CreateRes where
  toJSON = genericToJSON stripLensPrefixOptions

----------
-- Update
----------
data UpdateReq =
  UpdateReq
  { _maxAllowed :: Maybe Int
  , _startTime  :: Maybe LocalTime
  , _endTime    :: Maybe LocalTime
  }
  deriving (Show, Generic, ToJSON, ToSchema)

instance FromJSON UpdateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data UpdateRes =
  UpdateRes
  { _quota :: Quota
  }
  deriving (Show, Generic, FromJSON, Default, ToSchema)

instance ToJSON UpdateRes where
  toJSON = genericToJSON stripLensPrefixOptions

data ListRes =
  ListRes
  { _quotas :: [Quota]
  }
  deriving (Show, Generic, FromJSON, Default, ToSchema)

instance ToJSON ListRes where
  toJSON = genericToJSON stripLensPrefixOptions


type GetRes = Quota
