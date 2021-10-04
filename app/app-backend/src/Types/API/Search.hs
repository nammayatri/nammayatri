module Types.API.Search where

import Data.Time (UTCTime)
import EulerHS.Prelude
import Types.Common

data SearchReq = SearchReq
  { transaction_id :: Text,
    startTime :: UTCTime,
    origin :: Stop,
    destination :: Stop,
    travellers :: [Traveller],
    fare :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype SearchRes = SearchRes
  { caseId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)
