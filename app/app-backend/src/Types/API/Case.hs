module Types.API.Case where

import Beckn.Types.Storage.Case
import Data.Default
import Data.Swagger
import EulerHS.Prelude

data CaseReq = CaseReq
  { _type :: CaseType,
    _limit :: Integer,
    _offset :: Integer,
    _status :: CaseStatus
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON CaseReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON CaseReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

type CaseListRes = [Case]

data UpdateCaseReq = UpdateCaseReq
  { _quote :: Maybe Double,
    _transporterChoice :: Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON UpdateCaseReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON UpdateCaseReq where
  toJSON = genericToJSON stripAllLensPrefixOptions
