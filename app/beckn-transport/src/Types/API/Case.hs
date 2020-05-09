module Types.API.Case where

import           Data.Default
import           Data.Swagger
import           Beckn.Types.Storage.Case
import           EulerHS.Prelude

data CaseReq = CaseReq
  { _type   :: CaseType,
    _limit  :: Integer,
    _offset :: Integer,
    _status :: CaseStatus
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON CaseReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON CaseReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

data ListRes = ListRes
  { _comments :: [Case]
  }
  deriving (Show, Generic, Default, ToSchema)

instance ToJSON ListRes where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON ListRes where
  parseJSON = genericParseJSON stripLensPrefixOptions
