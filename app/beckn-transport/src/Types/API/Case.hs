module Types.API.Case where

import Beckn.Types.Amount
import Beckn.Utils.JSON
import Data.Swagger hiding (name)
import EulerHS.Prelude hiding (id)
import Types.Storage.Case
import Types.Storage.Location

data CaseRes = CaseRes
  { _case :: Case,
    fromLocation :: Location,
    toLocation :: Location
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
  deriving (Show, Generic, ToSchema)

instance FromJSON UpdateCaseReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON UpdateCaseReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data ProviderStats = ProviderStats
  { completed :: Maybe Int,
    inprogress :: Maybe Int,
    confirmed :: Maybe Int
  }
  deriving (Generic, Show)

instance FromJSON ProviderStats where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ProviderStats where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data ProviderInfo = ProviderInfo
  { id :: Text,
    name :: Text,
    stats :: Text,
    contacts :: Text
  }
  deriving (Generic, Show)

instance FromJSON ProviderInfo where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ProviderInfo where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
