module Types.API.Case where

import           Data.Default
import           Data.Swagger
import           Beckn.Types.Storage.Case
import           Beckn.Types.Storage.Location
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


data CaseRes = CaseRes
  { _case   :: Case,
    _fromLocation  :: Location,
    _toLocation  :: Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON CaseRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON CaseRes where
  toJSON = genericToJSON stripAllLensPrefixOptions

type CaseListRes = [CaseRes]

data UpdateCaseReq = UpdateCaseReq
  { _quote             :: Maybe Double
  , _transporterChoice :: Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON UpdateCaseReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON UpdateCaseReq where
  toJSON = genericToJSON stripAllLensPrefixOptions