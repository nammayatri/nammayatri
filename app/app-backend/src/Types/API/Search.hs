module Types.API.Search where

import Beckn.Types.Core.Error
import Data.Time (UTCTime)
import EulerHS.Prelude
import Types.API.Common
import Types.Common

data AckResponse = AckResponse
  { transaction_id :: Text,
    message :: Ack,
    error :: Maybe Error
  }
  deriving (Show, FromJSON, ToJSON, Generic)

data SearchReq = SearchReq
  { transaction_id :: Text,
    startTime :: UTCTime,
    origin :: Stop,
    destination :: Stop,
    vehicle :: Vehicle,
    travellers :: [Traveller],
    fare :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show)
