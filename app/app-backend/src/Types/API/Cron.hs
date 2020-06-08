module Types.API.Cron where

import Beckn.Types.App
import Beckn.Types.Common as BC
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Location as SL
import Beckn.Utils.Extra
import Data.Swagger
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant.Swagger

data ExpireCaseReq = ExpireCaseReq
  { from :: Maybe LocalTime,
    to :: Maybe LocalTime
  }
  deriving (Generic, ToSchema, ToJSON, Show, FromJSON)

data ExpireCaseRes = ExpireCaseRes
  { updated_count :: Int
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)
