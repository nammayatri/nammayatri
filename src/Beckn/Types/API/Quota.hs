module Beckn.Types.API.Quota where

import           Beckn.Types.Common
import           Beckn.Types.Storage.Quota
import           Data.Default
import           Data.Time.LocalTime
import           EulerHS.Prelude
----------
-- Create
----------
data CreateReq =
  CreateReq
  { _maxAllowed :: Int
  , _type       :: QuotaType
  , _EntityId   :: Text
  , _entityType :: EntityType
  , _startTime  :: LocalTime
  , _endTime    :: LocalTime
  }
  deriving (Show, Generic, ToJSON)

instance FromJSON CreateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions


data CreateRes =
  CreateRes
  { _quota :: Quota
  }
  deriving (Show, Generic, FromJSON, Default)

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
  deriving (Show, Generic, ToJSON)

instance FromJSON UpdateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data UpdateRes =
  UpdateRes
  { _quota :: Quota
  }
  deriving (Show, Generic, FromJSON, Default)

instance ToJSON UpdateRes where
  toJSON = genericToJSON stripLensPrefixOptions

data ListRes =
  ListRes
  { _quotas :: [Quota]
  }
  deriving (Show, Generic, FromJSON, Default)

instance ToJSON ListRes where
  toJSON = genericToJSON stripLensPrefixOptions


type GetRes = Quota
