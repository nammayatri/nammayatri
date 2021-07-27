module Types.API.Case where

import Beckn.Types.Amount
import Beckn.Utils.JSON
import Data.Swagger hiding (name)
import EulerHS.Prelude hiding (id)
import Types.Storage.Case
import Types.Storage.SearchReqLocation

data CaseRes = CaseRes
  { _case :: Case,
    fromLocation :: SearchReqLocation,
    toLocation :: SearchReqLocation
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON CaseRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON CaseRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

type CaseListRes = [CaseRes]

data UpdateCaseReq = UpdateCaseReq
  { quote :: Maybe Amount,
    transporterChoice :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data ProviderStats = ProviderStats
  { completed :: Maybe Int,
    inprogress :: Maybe Int,
    confirmed :: Maybe Int
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data ProviderInfo = ProviderInfo
  { id :: Text,
    name :: Text,
    stats :: Text,
    contacts :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)
