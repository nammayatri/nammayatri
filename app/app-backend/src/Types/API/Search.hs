module Types.API.Search where

import Beckn.Types.Id
import EulerHS.Prelude hiding (id)
import Types.Storage.Case (Case)
import Types.Storage.SearchReqLocation (SearchReqLocationAPIEntity)

data VehicleVariant = SEDAN | SUV | HATCHBACK
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data SearchReq = SearchReq
  { origin :: SearchReqLocationAPIEntity,
    destination :: SearchReqLocationAPIEntity,
    vehicle :: VehicleVariant
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype SearchRes = SearchRes
  { searchId :: Id Case
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data CaseInfo = CaseInfo
  { total :: Maybe Integer,
    accepted :: Maybe Integer,
    declined :: Maybe Integer
  }
  deriving (Show, Generic, FromJSON, ToJSON)