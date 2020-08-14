module Types.API.Cron where

import Data.Swagger
import Data.Time
import EulerHS.Prelude

data ExpireCaseReq = ExpireCaseReq
  { from :: UTCTime,
    to :: UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, Show, FromJSON)

newtype ExpireCaseRes = ExpireCaseRes
  { updated_count :: Int
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)
