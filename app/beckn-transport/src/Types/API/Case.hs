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

type CaseListRes = [Case]