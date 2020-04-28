module Beckn.Types.API.Quota where

import           Beckn.Types.Storage.Quota
import           Data.Time.LocalTime
import           EulerHS.Prelude
----------
-- Create
----------
data CreateReq =
  CreateReq
  { _maxAllowed :: Int
  , _type       :: Text
  , _EntityId   :: Text
  , _entityType :: Text
  , _startTime  :: LocalTime
  , _endTime    :: LocalTime
  }
  deriving (Show, Generic, ToJSON)

data CreateRes =
  CreateRes
  { _quota :: Quota
  }
  deriving (Show, Generic, FromJSON)

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

data UpdateRes =
  UpdateRes
  { _quota :: Quota
  }
  deriving (Show, Generic, FromJSON)

----------
-- List
----------
-- _limit, _offset and _type
-- present in queryParams

data ListRes =
  ListRes
  { _quotas :: [Quota]
  }
  deriving (Show, Generic, FromJSON)
