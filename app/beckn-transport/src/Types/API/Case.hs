module Types.API.Case where

import Beckn.Types.Amount
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Location
import Data.Swagger
import EulerHS.Prelude

data CaseRes = CaseRes
  { _case :: Case,
    _fromLocation :: Location,
    _toLocation :: Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON CaseRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON CaseRes where
  toJSON = genericToJSON stripAllLensPrefixOptions

type CaseListRes = [CaseRes]

data UpdateCaseReq = UpdateCaseReq
  { _quote :: Maybe Amount,
    _transporterChoice :: Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON UpdateCaseReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON UpdateCaseReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

data ProviderStats = ProviderStats
  { _completed :: Maybe Int,
    _inprogress :: Maybe Int,
    _confirmed :: Maybe Int
  }
  deriving (Generic, Show)

instance FromJSON ProviderStats where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ProviderStats where
  toJSON = genericToJSON stripAllLensPrefixOptions

data ProviderInfo = ProviderInfo
  { _id :: Text,
    _name :: Text,
    _stats :: Text,
    _contacts :: Text
  }
  deriving (Generic, Show)

instance FromJSON ProviderInfo where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ProviderInfo where
  toJSON = genericToJSON stripAllLensPrefixOptions
