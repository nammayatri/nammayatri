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

data CreateRes =
  CreateRes
  { _quota :: Quota
  }
  deriving (Show, Generic, FromJSON, Default)

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
  deriving (Show, Generic, FromJSON, Default)

----------
-- List
----------
-- _limit, _offset and _type
-- present in queryParams

data ListRes =
  ListRes
  { _quotas :: [Quota]
  }
  deriving (Show, Generic, FromJSON, Default)
