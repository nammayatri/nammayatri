module Types.API.Cron where

import Beckn.Types.App
import Beckn.Utils.Extra
import Data.Swagger
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant.Swagger

data ExpireCaseReq = ExpireCaseReq
  { from :: LocalTime,
    to :: LocalTime
  }
  deriving (Generic, ToSchema, ToJSON, Show, FromJSON)

newtype ExpireCaseRes = ExpireCaseRes
  { updated_count :: Int
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)
