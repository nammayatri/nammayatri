module Types.API.Cron where

import Data.Swagger
import Data.Time.LocalTime
import EulerHS.Prelude

data ExpireCaseReq = ExpireCaseReq
  { from :: LocalTime,
    to :: LocalTime
  }
  deriving (Generic, ToSchema, ToJSON, Show, FromJSON)

newtype ExpireCaseRes = ExpireCaseRes
  { updated_count :: Int
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)
