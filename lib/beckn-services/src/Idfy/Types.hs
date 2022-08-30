{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Idfy.Types where

import Beckn.Types.Error.BaseError.HTTPError hiding (Error)
import Beckn.Types.Error.BaseError.HTTPError.FromResponse
import Beckn.Utils.JSON
import Data.Aeson hiding (Error)
import Data.OpenApi
import EulerHS.Prelude hiding (state)

type AccountId = Text

type ApiKey = Text

data ValidateReq = ValidateRCReq
  {
      task_id :: Text,
      group_id :: Text,
      _data :: ValidateDoc
  }
  deriving (Show, Generic)

instance ToSchema ValidateReq where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON ValidateReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ValidateReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
data ValidateDoc =  ValidateDoc
  {
    document1 :: Text,
    doc_type :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
 
data ValidateRes =  ValidateRCRes
  {
      action :: Maybe Text,
      completed_at :: Maybe Text,
      created_at :: Maybe Text,
      group_id :: Maybe Text,
      request_id :: Maybe Text,
      result :: ResultBody,
      status :: Text,
      task_id :: Maybe Text,
      _type :: Maybe Text
  } 
  deriving (Show, Generic)

instance ToSchema ValidateRes where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON ValidateRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ValidateRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny  

data ResultBody = ResultBody
  {
    detected_doc_type :: Maybe Text,
    is_readable :: Maybe Bool,
    readability :: ReadabilityBody
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

newtype ReadabilityBody = ReadabilityBody
  {
    confidence :: Maybe Int
  }
  deriving (Show, Generic)

deriving newtype instance ToJSON ReadabilityBody

deriving newtype instance FromJSON ReadabilityBody

deriving newtype instance ToSchema ReadabilityBody  


data VerifyDLReq = VerifyDLReq
  { task_id :: Text,
    group_id :: Text,
    _data :: VerifyDocData
  }
  deriving (Show, Generic)

instance ToSchema VerifyDLReq where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON VerifyDLReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON VerifyDLReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data VerifyDLData = VerifyDLData
  { id_number :: Text,
    date_of_birth :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data VerifyDLwithSourceReq = VerifyDLwithSourceReq
  { task_id :: Text,
    group_id :: Text,
    _data :: VerifyDLData
  }
  deriving (Show, Generic)

instance ToSchema VerifyDLwithSourceReq where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON VerifyDLwithSourceReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON VerifyDLwithSourceReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data VerifyDocData = VerifyDLwithSourceData
  { document1 :: Text,
    document2 :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data VerifyRCReq = VerifyRCReq
  { task_id :: Text,
    group_id :: Text,
    _data :: VerifyDocData
  }
  deriving (Show, Generic)

instance ToSchema VerifyRCReq where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON VerifyRCReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON VerifyRCReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype VerifyDLAsyncData = VerifyRCData {request_id :: Text}
  deriving (Show, Generic)

deriving newtype instance ToJSON VerifyDLAsyncData

deriving newtype instance FromJSON VerifyDLAsyncData

deriving newtype instance ToSchema VerifyDLAsyncData

data VerifyIdfyRCAsyncRes = VerifyIdfyRCAsyncRes
  { trade_name :: Maybe Text,
    chassis_number :: Maybe Text,
    vehicle_class :: Maybe Text,
    insurance_details :: Maybe Text,
    string :: Maybe Text,
    insurance_validity :: Maybe Text,
    permit_type :: Maybe Text,
    permit_validity :: Maybe Text,
    permit_type :: Maybe Text,
    engine_number :: Maybe Text,
    vehicle_financier :: Maybe Text,
    fitness_upto :: Maybe Text,
    fuel_type :: Maybe Text,
    manufacturer :: Maybe Text,
    last_updated_date :: Maybe Text,
    owner_name :: Maybe Text,
    registration_date :: Maybe Text,
    registration_number :: Maybe Text,
    rto_name :: Maybe Text,
    status :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data VerifyRCwithSourceReq = VerifyRCwithSourceReq
  { task_id :: Text,
    group_id :: Text,
    _data :: VerifyRCwithSourceData
  }
  deriving (Show, Generic)

instance ToSchema VerifyRCwithSourceReq where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON VerifyRCwithSourceReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON VerifyRCwithSourceReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype VerifyRCwithSourceData = VerifyRCwithSourceData
  { rc_number :: Text
  }
  deriving (Show, Generic)

deriving newtype instance ToJSON VerifyRCwithSourceData

deriving newtype instance FromJSON VerifyRCwithSourceData

deriving newtype instance ToSchema VerifyRCwithSourceData  


data VerifyRCSyncRes = VerifyRCSyncRes
  { address :: Maybe Text,
    body :: Maybe Text,
    chassis_number :: Maybe Text,
    _class :: Maybe Text,
    colour :: Maybe Text,
    cubic_capacity :: Maybe Text,
    document1_side :: Maybe Text,
    document2_side :: Maybe Text,
    engine_number :: Maybe Text,
    fathers_name :: Maybe Text,
    fuel :: Maybe Text,
    manufacturer :: Maybe Text,
    manufacturing_date :: Maybe Text,
    model :: Maybe Text,
    owner_name :: Maybe Text,
    registration_date :: Maybe Text,
    registration_number :: Maybe Text,
    rto_district :: Maybe Text,
    state :: Maybe Text,
    wheel_base :: Maybe Text,
    status :: Text
  }
  deriving (Show, Generic)

instance ToSchema VerifyRCSyncRes where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON VerifyRCSyncRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON VerifyRCSyncRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data COV = W_NT | W_T | W_CAB | HGV_T | HMV_HGV | HMV | HTV | LMV | LMV_NT | LMV_T | LMV_CAB | LMV_HMV | LTV | MCWG | MCWOG | HPMV | MGV | MMV | LDRXCV | PSV_BUS | TRANS | TRCTOR | Others
  deriving (Show, Generic, ToSchema)

instance FromJSON COV where
  parseJSON = genericParseJSON constructorForCOVFromJson

instance ToJSON COV where
  toJSON = genericToJSON constructorForCOVToJson

constructorForCOVFromJson :: Options
constructorForCOVFromJson =
  defaultOptions
    { constructorTagModifier = \case
        "3W_NT" -> "W_NT"
        "3W_T" -> "W_T"
        "3W_CAB" -> "W_CAB"
        val -> val
    }

constructorForCOVToJson :: Options
constructorForCOVToJson =
  defaultOptions
    { constructorTagModifier = \case
        "W_NT" -> "3W_NT"
        "W_T" -> "3W_T"
        "W_CAB" -> "3W_CAB"
        val -> val
    }

data VerifyDLSyncRes = VerifyDLSyncRes
  { id_number :: Maybe Text,
    name_on_card :: Maybe Text,
    fathers_name :: Maybe Text,
    date_of_birth :: Maybe Text,
    date_of_validity :: Maybe Text,
    address :: Maybe Text,
    district :: Maybe Text,
    street_address :: Maybe Text,
    pincode :: Maybe Text,
    state :: Maybe Text,
    issue_dates :: ValidateIssueDate,
    _type :: [COV],
    validity :: Validity,
    status :: Text
  }
  deriving (Show, Generic)

instance ToSchema VerifyDLSyncRes where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON VerifyDLSyncRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON VerifyDLSyncRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Validity = Validity
  { nt :: Maybe Text,
    t :: Maybe Text
  }
  deriving (Show, Generic)

data ValidateIssueDate = ValidateIssueDate
  { lmv :: Maybe Text,
    mcwg :: Maybe Text,
    trans :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema Validity where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions constructorsToUpperOptions

instance FromJSON Validity where
  parseJSON = genericParseJSON constructorsToUpperOptions

instance ToJSON Validity where
  toJSON = genericToJSON constructorsToUpperOptions

instance ToSchema ValidateIssueDate where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions constructorsToUpperOptions

instance FromJSON ValidateIssueDate where
  parseJSON = genericParseJSON constructorsToUpperOptions

instance ToJSON ValidateIssueDate where
  toJSON = genericToJSON constructorsToUpperOptions

newtype Error = Error {message :: Text}
  deriving (Show, Generic)

instance IsAPIError Error

deriving newtype instance ToJSON Error

deriving newtype instance FromJSON Error

deriving newtype instance ToSchema Error

instance FromResponse Error where
  fromResponse = fromJsonResponse

instance IsBaseError Error where
  toMessage _ = Just "IDFY_VERIFICATION_UNAVAILABLE"

instance IsHTTPError Error where
  toErrorCode _ = "CORE002"
  toHttpCode _ = E404

instance IsBecknAPIError Error where
  toType _ = DOMAIN_ERROR

instanceExceptionWithParent 'HTTPException ''Error
