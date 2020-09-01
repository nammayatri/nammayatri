module Types.API.Cron where

import Data.Swagger
import Data.Time
import EulerHS.Prelude

data ExpireCaseReq = ExpireCaseReq
  { from :: Maybe UTCTime,
    to :: Maybe UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, Show, FromJSON)

newtype ExpireRes = ExpireRes
  { updated_count :: Int
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)
