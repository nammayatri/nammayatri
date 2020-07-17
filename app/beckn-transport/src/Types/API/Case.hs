module Types.API.Case where

import Beckn.Types.Core.Amount
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
