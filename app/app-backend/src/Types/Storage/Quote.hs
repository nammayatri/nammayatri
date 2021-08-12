module Types.Storage.Quote where

import Beckn.Types.Amount (Amount)
import Beckn.Types.Id
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)
import Types.Storage.ProductInstance (ProductInstance)
import Data.OpenApi (ToSchema)

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id ProductInstance,
    estimatedPrice :: Amount,
    agencyName :: Text,
    agencyNumber :: Text,
    agencyCompletedRidesCount :: Int,
    nearestDriverDistance :: Double,
    createdAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)